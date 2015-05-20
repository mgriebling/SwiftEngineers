//
//  BigNumbers.swift
//  TestEngineerMath
//
//  Created by Michael Griebling on 19May2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

struct BigReal {
	var exponent: Int = 0
	var number = [Int]()
	
	static let Bits = 30
	static let Radix = 1 << Bits
	let radix = BigReal.Radix
	let fradix = Double(BigReal.Radix)
	
	init() {}  // default number is zero
	
	init (_ d: Double) {
		let (num, exp) = frexp(d)
		exponent = exp
		let int0 = Int(num * fradix)
		if int0 != 0 {
			number.append(int0); exponent -= BigReal.Bits
			let n0 = num * fradix - Double(int0)
			let int1 = Int(n0 * fradix)
			if int1 != 0 {
				number.append(int1); exponent -= BigReal.Bits
			}
			normalize()
		}
	}
	
	mutating func normalize () {
		if var n = number.last {
			while n & 1 == 0 {
				n >>= 1; exponent++
			}
			self.number[self.number.count-1] = n
		}
	}
	
	// conversions to basic types
	var double : Double {
		return 0
	}
	
	func add (num: BigReal) -> BigReal {
		return BigReal()
	}
}