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

public class BigReal : Printable, Comparable, Equatable, Hashable {
	
	private var number = mpf_t()	// signed extended floating-point number
	
	public static var bitSize: Int {
		set (newSize) {
			gmp.setBitSize(newSize)
		}
		get {
			return gmp.getBitSize()
		}
	}

	public var description : String {
		let str = gmp.getStringFrom(&number).stringByTrimmingTrailingCharactersInSet(NSCharacterSet(charactersInString: "0"))
		return str.stringByTrimmingTrailingCharactersInSet(NSCharacterSet(charactersInString: "."))
	}
	
	public var hashValue : Int {
		return self.integer
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
	
	public func cmp (n: BigReal) -> NSComparisonResult {
		return gmp.cmp(&number, toNumber: &n.number)
	}
	
	public func ipower (n: Int) -> BigReal {
		var temp = mpf_t()
		gmp.ipower(&number, toPower: n, giving: &temp)
		return BigReal(temp)
	}
}

public func + (lhs: BigReal, rhs: BigReal) -> BigReal { return lhs.add(rhs) }
public func + (lhs: BigReal, rhs: Double) -> BigReal { return lhs.add(BigReal(rhs)) }
public func + (lhs: Double, rhs: BigReal) -> BigReal { return BigReal(lhs).add(rhs) }

public func - (lhs: BigReal, rhs: BigReal) -> BigReal { return lhs.sub(rhs) }
public func - (lhs: BigReal, rhs: Double) -> BigReal { return lhs.sub(BigReal(rhs)) }
public func - (lhs: Double, rhs: BigReal) -> BigReal { return BigReal(lhs).sub(rhs) }

public func * (lhs: BigReal, rhs: BigReal) -> BigReal { return lhs.mul(rhs) }
public func * (lhs: BigReal, rhs: Double) -> BigReal { return lhs.mul(BigReal(rhs)) }
public func * (lhs: Double, rhs: BigReal) -> BigReal { return BigReal(lhs).mul(rhs) }

public func / (lhs: BigReal, rhs: BigReal) -> BigReal { return lhs.div(rhs) }
public func / (lhs: BigReal, rhs: Double) -> BigReal { return lhs.div(BigReal(rhs)) }
public func / (lhs: Double, rhs: BigReal) -> BigReal { return BigReal(lhs).div(rhs) }

public func ** (lhs: BigReal, rhs: Int) -> BigReal { return lhs.ipower(rhs) }
public func ** (lhs: Double, rhs: Int) -> BigReal { return BigReal(lhs).ipower(rhs) }
public func ** (lhs: Int, rhs: Int) -> BigReal { return BigReal(lhs).ipower(rhs) }

public func == (lhs: BigReal, rhs: BigReal) -> Bool { return lhs.cmp(rhs) == .OrderedSame }
public func < (lhs: BigReal, rhs: BigReal) -> Bool { return lhs.cmp(rhs) == .OrderedAscending }

