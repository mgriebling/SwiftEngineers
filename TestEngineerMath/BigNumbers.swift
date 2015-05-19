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
		let int0 = Int(num * fradix)
		let n0 = num * fradix - Double(int0)
		let int1 = Int(n0 * fradix)
		let n1 = n0 * fradix - Double(int1)
	}
	
	// conversions to basic types
	var double : Double {
		return 0
	}
	
	func add (num: BigReal) -> BigReal {
		return BigReal()
	}
}