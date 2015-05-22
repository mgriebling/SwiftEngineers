//
//  BigNumbers.swift
//  TestEngineerMath
//
//  Created by Michael Griebling on 19 May 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

class BigReal : Printable {
	// Note: needs to be a class so the mpz memory can be released via deinit
	var exponent: Int = 0	// base 2 exponent
	var number = mpz_t()	// signed integer number
	
	var description : String {
		var str = ""
		var work = ""
		var copy = number
		var rem = mpz_t()
		
		func addDigit() {
			let digit = mpz_tdiv_q_ui(&copy, &copy, 10)
			work = "\(digit)" + work
		}
		
		let pre = getRawString(copy)
		let bits = mpz_sizeinbase(&copy, 2)
		if isNegative() { str = "-"; mpz_abs(&copy, &copy) }
		mpz_init2(&rem, UInt(bits))
		if exponent < 0 {
			var scale = UInt(-exponent)
			let exp10 = Double(scale) * log(2.0) / log(10.0) + 5  // add extra digits
			var n = mpz_t()
			mpz_init2(&n, scale+10)
			mpz_ui_pow_ui(&n, UInt(10), UInt(exp10))
			mpz_tdiv_r_2exp(&rem, &copy, scale)
			mpz_mul(&rem, &rem, &n)
			mpz_tdiv_q_2exp(&rem, &rem, scale)
			mpz_tdiv_q_2exp(&copy, &copy, scale)
			mpz_clear(&n)
		} else {
			mpz_mul_2exp(&copy, &copy, UInt(exponent))
		}
		let post = getRawString(copy)
		let rpost = getRawString(rem)
		
		// get predecimal digits
		while mpz_cmp_ui(&copy, 0) > 0 { addDigit() }
		
		// get postdecimal digits
		if work.isEmpty { work = "0" }
		mpz_set(&copy, &rem)
		str += work + "."; work = ""
		while mpz_cmp_ui(&copy, 0) > 0 { addDigit() }

		// clean up
		mpz_clear(&copy); mpz_clear(&rem)
		return str + work
	}
	
	private func getRawString (var num: mpz_t) -> String {
		var cstr = [CChar](count: 1024, repeatedValue: 0)
		mpz_get_str(&cstr, 10, &num)
		return NSString(CString: cstr, encoding: NSASCIIStringEncoding) as! String
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
		var negative = false
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
		mpz_init_set_si(&number, int)
	}
	
	init (_ s: String) {
		var sNoBlanks = s.stringByTrimmingCharactersInSet(NSCharacterSet.whitespaceAndNewlineCharacterSet())
		var negative = false
		
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
				if let exp10t = sNoBlanks.toInt() { exp10 += exp10t }
			}
			
			// scale number by exponent
			let expNegative = exp10 < 0; exp10 = abs(exp10)
			let exp2 = Double(exp10) * log(10.0) / log(2.0) + 0.5
			var n = mpz_t()
			var scale: UInt = 0
			while scale < UInt(exp2) { scale += BigReal.Bits }
			scale += BigReal.Bits    // add some extra precisio
			mpz_init2(&n, scale+10)
			mpz_ui_pow_ui(&n, UInt(10), UInt(exp10))
			if expNegative {
				// scale number by 2^scale / 10^exp10 where 2^scale > 10^exp10
				mpz_mul_2exp(&number, &number, scale)
				mpz_cdiv_q(&number, &number, &n)
				exponent -= Int(scale)
			} else {
				mpz_mul(&number, &number, &n)
			}
			mpz_clear(&n)
			normalize()
			if negative { mpz_neg(&number, &number) }
		}
	}
	
	 func normalize () {
		while mpz_tstbit (&number, 0) == 0 {
			mpz_cdiv_q_2exp(&number, &number, 1)
			exponent++
		}
	}
	
	func isNegative () -> Bool {
		return mpz_cmp_ui(&number, 0) < 0
	}
	
	// conversions to basic types
	var double : Double {
		var n = 0.0
		var work = number
		var exp = exponent
		mpz_abs(&work, &work)
		while mpz_cmp_ui(&work, 0) != 0 {
			let int = mpz_tdiv_q_ui(&work, &work, radix)
			if n != 0 { exp += Int(BigReal.Bits) }
			n = n / fradix + Double(int)
		}
		mpz_clear(&work)
		return isNegative() ? -ldexp(n, exp) : ldexp(n, exp)
	}
	
	var integer : Int {
		var work = number
		var res : Int
		if exponent < 0 {
			mpz_cdiv_q_2exp(&work, &work, UInt(-exponent))
		} else {
			mpz_mul_2exp(&work, &work, UInt(exponent))
		}
		if mpz_fits_slong_p(&work) != 0 { res = mpz_get_si(&work) }
		else if mpz_cmp_ui(&work, 0) < 0 { res = Int.min }
		else { res = Int.max }
		mpz_clear(&work)
		return res
	}
	
	func add (num: BigReal) -> BigReal {
		return BigReal()
	}
}