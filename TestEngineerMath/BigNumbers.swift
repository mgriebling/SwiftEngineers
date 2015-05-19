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
		let n0 = num * fradix - Double(int0)
		let int1 = Int(n0 * fradix)
		let n1 = n0 * fradix - Double(int1)
		let int2 = Int(n1 * fradix)
		let n2 = n1 * fradix - Double(int2)
		if int0 != 0 {
			number.append(int0); exponent -= BigReal.Bits
			if int1 != 0 {
				number.append(int1); exponent -= BigReal.Bits
				if int2 != 0 {
					number.append(int2); exponent -= BigReal.Bits
				}
			}
		}
	}
	
	func normalize () {
		
	}
	
	// conversions to basic types
	var double : Double {
		return 0
	}
	
	func add (num: BigReal) -> BigReal {
		return BigReal()
	}
}