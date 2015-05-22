//
//  BigNumbers.swift
//  TestEngineerMath
//
//  Created by Michael Griebling on 19 May 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

extension String {
	
	func stringByTrimmingTrailingCharactersInSet (characterSet: NSCharacterSet) -> String {
		if let rangeOfLastWantedCharacter = self.rangeOfCharacterFromSet(characterSet.invertedSet, options:.BackwardsSearch) {
			return self.substringToIndex(rangeOfLastWantedCharacter.endIndex)
		}
		return ""
	}
}

public class BigReal : Printable {
	
	private var number = mpf_t()	// signed extended floating-point number

	public var description : String {
		let str = gmp.getStringFrom(&number)
		if str.hasPrefix(".") { return "0" + str }
		return str.stringByTrimmingTrailingCharactersInSet(NSCharacterSet(charactersInString: "."))
	}
	
	public init() {
		// default number is zero
		gmp.setNumber(&number, fromInt: 0)
	}
	
	public init (_ d: Double) {
		gmp.setNumber(&number, fromDouble: d)
	}
	
	public init (_ int: Int) {
		gmp.setNumber(&number, fromInt: int)
	}
	
	private init (var _ m: mpf_t) {
		gmp.setNumber(&number, fromLongDouble: &m)
	}
	
	public init (_ s: String) {
		gmp.setNumber(&number, fromString: s)
	}
	
	// conversions to basic types
	public var double : Double {
		return gmp.getDoubleFrom(&number)
	}
	
	public var integer : Int {
		return gmp.getIntegerFrom(&number)
	}
	
	public func add (n: BigReal) -> BigReal {
		var temp = mpf_t()
		gmp.add(&number, toNumber: &n.number, giving: &temp)
		return BigReal(temp)
	}
	
	public func sub (n: BigReal) -> BigReal {
		var temp = mpf_t()
		gmp.sub(&number, toNumber: &n.number, giving: &temp)
		return BigReal(temp)
	}
	
	public func mul (n: BigReal) -> BigReal {
		var temp = mpf_t()
		gmp.mul(&number, toNumber: &n.number, giving: &temp)
		return BigReal(temp)
	}
	
	public func div (n: BigReal) -> BigReal {
		var temp = mpf_t()
		gmp.div(&number, toNumber: &n.number, giving: &temp)
		return BigReal(temp)
	}
}

func + (lhs: BigReal, rhs: BigReal) -> BigReal {
	return lhs.add(rhs)
}

func - (lhs: BigReal, rhs: BigReal) -> BigReal {
	return lhs.sub(rhs)
}

func * (lhs: BigReal, rhs: BigReal) -> BigReal {
	return lhs.mul(rhs)
}

func / (lhs: BigReal, rhs: BigReal) -> BigReal {
	return lhs.div(rhs)
}


