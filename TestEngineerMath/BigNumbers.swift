//
//  BigNumbers.swift
//  TestEngineerMath
//
//  Created by Michael Griebling on 19 May 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

struct BigReal : Printable {
	var exponent: Int = 0
	var negative = false
	var number = mpz_t()
	
	var description : String {
		var str = [CChar](count: 1024, repeatedValue: 0)
		var copy = number
		mpz_get_str(&str, 10, &copy)
		if let nstr = NSString(CString: str, encoding: NSASCIIStringEncoding) {
			return nstr as String
		}
		return ""
	}
	
	static let Bits : UInt = 30
	static let Radix = 1 << Bits
	let radix = BigReal.Radix
	let fradix = Double(BigReal.Radix)
	
	init() {
		// default number is zero
		mpz_init(&number)
	}
	
	init (_ d: Double) {
		var (num, exp) = frexp(d)
		mpz_init2(&number, UInt(16*sizeof(UInt)))
		if num < 0 { num = -num; negative = true }
		exponent = exp
		let int0 = UInt(num * fradix)
		if int0 != 0 {
			mpz_add_ui(&number, &number, int0)
			exponent -= Int(BigReal.Bits)
			let n0 = num * fradix - Double(int0)
			let int1 = UInt(n0 * fradix)
			if int1 != 0 {
				mpz_mul_2exp(&number, &number, BigReal.Bits)
				mpz_add_ui(&number, &number, int1)
				exponent -= Int(BigReal.Bits)
			}
			normalize()
		}
	}
	
	init (_ uint: UInt) {
		mpz_init_set_ui(&number, uint)
	}
	
	init (_ int: Int) {
		var li = int
		if int < 0 { negative = true; li = -li }
		mpz_init_set_si(&number, li)
	}
	
	init (_ s: String) {
		
		var sNoBlanks = s.stringByTrimmingCharactersInSet(NSCharacterSet.whitespaceAndNewlineCharacterSet())
		
		func getChar() -> Character {
			if sNoBlanks.isEmpty { return "\0" }
			return sNoBlanks.removeAtIndex(sNoBlanks.startIndex)
		}
		
		func addToNumber(c: Character) {
			let s = "" + [c]
			mpz_mul_ui(&number, &number, 10)
			mpz_add_ui(&number, &number, UInt(s.toInt()!))
		}
		
		func isDigit (c: Character) -> Bool { return c >= "0" && c <= "9" }
		
		mpz_init2(&number, UInt(sNoBlanks.lengthOfBytesUsingEncoding(NSASCIIStringEncoding))/8 + 1)
		if !sNoBlanks.isEmpty {
			var char = getChar()
			var exp10 = 0
			if char == "+" { char = getChar() }
			else if char == "-" { negative = true; char = getChar() }
			
			// predecimal digits
			while isDigit(char) { addToNumber(char); char = getChar() }
			
			if char == "." {
				char = getChar()
				
				// postdecimal digits
				while isDigit(char) { addToNumber(char); char = getChar(); exp10-- }
			}
			
			// get any exponent
			if char == "e" || char == "E" {
				char = getChar()
				if let exp10t = sNoBlanks.toInt() { exp10 += exp10t }
			}
			
			// convert to base 2 exponent
			let exp2 = Double(exp10) * log(10.0) / log(2.0)
			var n = mpz_t()
			mpz_init2(&n, UInt(abs(exp2)+10.0))
			let expNegative = exp10 < 0
			exp10 = abs(exp10)
			mpz_ui_pow_ui(&n, UInt(10), UInt(exp10))
			if expNegative {
				mpz_mul_2exp(&number, &number, BigReal.Bits)
				mpz_cdiv_q(&number, &number, &n)
				exponent -= Int(BigReal.Bits)
			} else {
				mpz_mul(&number, &number, &n)
			}
			normalize()
		}
	}
	
	mutating func normalize () {
		while mpz_tstbit (&number, 0) == 0 {
			mpz_cdiv_q_2exp(&number, &number, 1)
			exponent++
		}
	}
	
	// conversions to basic types
	var double : Double {
		var n = 0.0
		var work = number
		var exp = exponent
		while mpz_cmp_ui(&work, 0) != 0 {
			let int = mpz_tdiv_q_ui(&work, &work, radix)
			if n != 0 { exp += Int(BigReal.Bits) }
			n = n / fradix + Double(int)
		}
		return negative ? -ldexp(n, exp) : ldexp(n, exp)
	}
	
	var integer : Int {
		var work = number
		if mpz_fits_slong_p(&work) != 0 {
			let n = mpz_get_si(&work)
			return negative ? -n : n
		}
		return negative ? Int.min : Int.max
	}
	
	func add (num: BigReal) -> BigReal {
		return BigReal()
	}
}