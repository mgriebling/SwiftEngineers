//
//  BigReal.swift
//  EngineerMath
//
//  Created by Michael Griebling on 19 May 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

public class BigReal : CustomStringConvertible, Comparable, Hashable {
	
	internal var number = BigFloat()	// signed extended floating-point number
	internal var re : BigReal {
		return BigReal(number)
	}
	static var radix : UInt16 = 10
	static var mode : BFTrigMode = .radians
	
	var floatingPointClass: FloatingPointClassification = .PositiveNormal
	var isSignMinus: Bool { return number.negative }
	var isNormal: Bool { return number.valid }
	var isFinite: Bool { return !isInfinite }
	var isZero: Bool { return number.zero }
	var isSubnormal: Bool { return false }
	var isInfinite: Bool { return floatingPointClass == .PositiveInfinity || floatingPointClass == .NegativeInfinity }
	var isNaN: Bool { return !number.valid || floatingPointClass == .QuietNaN }
	var isSignaling: Bool { return !number.valid }

	public var description : String {
		return number.toString
	}
	
	public var hashValue : Int {
		return number.hashValue
	}
	
	public init() {
		// default number is zero
		number = BigFloat(int: 0, radix: BigReal.radix)
	}
	
	public init (_ d: Double) {
		number = BigFloat(double: d, radix: BigReal.radix)
	}
	
	public init (_ int: Int) {
		number = BigFloat(int: Int32(int), radix: BigReal.radix)
	}
	
	internal init (_ m: BigFloat) {
		number.assign(m)
	}
	
	public init (_ s: String) {
		number = BigFloat(string: s, radix: BigReal.radix)
	}
	
	// conversions to basic types
	public var double : Double {
		return number.doubleValue
	}
	
	public var integer : Int {
		return Int(number.doubleValue)
	}
	
	func isInteger () -> Bool {
		return number.isInteger()
	}
	
	public func add (n: BigReal) -> BigReal {
		let x = icopy()
		x.add(n.number)
		return BigReal(x)
	}
	
	public func sub (n: BigReal) -> BigReal {
		let x = icopy()
		x.subtract(n.number)
		return BigReal(x)
	}
	
	public func mul (n: BigReal) -> BigReal {
		let x = icopy()
		x.multiplyBy(n.number)
		return BigReal(x)
	}
	
	public func div (n: BigReal) -> BigReal {
		let x = icopy()
		x.divideBy(n.number)
		return BigReal(x)
	}
	
	public func cmp (n: BigReal) -> NSComparisonResult {
		return number.compareWith(n.number)
	}
	
	public func ipower (n: Int) -> BigReal {
		let x = icopy()
		x.raiseToIntPower(n)
		return BigReal(x)		
	}
	
	func cos () -> BigReal {
		let x = icopy()
		x.abs()
		return BigReal(x)
	}
	
	var abs:BigReal {
		let x = icopy()
		x.abs()
		return BigReal(x)
	}

	func exp()->BigReal {
		let x = icopy()
		x.powerOfE()
		return BigReal(x)
	}
	
	func ln()->BigReal {
		let x = icopy()
		x.ln()
		return BigReal(x)
	}
	
	func log()->BigReal {
		let x = icopy()
		x.logOfBase(BigFloat(int: 10, radix: BigReal.radix))
		return BigReal(x)
	}
	
	func sin()->BigReal {
		let x = icopy()
		x.sinWithTrigMode(BigReal.mode, inv: false, hyp: false)
		return BigReal(x)
	}
	
	func sqrt()->BigReal {
		let x = icopy()
		x.sqrt()
		return BigReal(x)
	}
	
	func atan()->BigReal {
		let x = icopy()
		x.tanWithTrigMode(BigReal.mode, inv: true, hyp: false)
		return BigReal(x)
	}
	
	internal func icopy() -> BigFloat {
		let x = BigFloat()
		x.assign(number)
		return x
	}
	
	func copy() -> BigReal {
		return BigReal(icopy())
	}
	
	func negate() -> BigReal {
		let x = BigFloat()
		x.assign(number)
		x.negate()
		return BigReal(x)
	}
	
	func atan2(y:BigReal)->BigReal {
		let x = copy()
		
		// Algorithm shamelessly stolen from qd_real
		if x.isZero {
			if y.isZero {
				// Actually an error -- how do I signal that?
				let x = BigReal(0)
				floatingPointClass = FloatingPointClassification.SignalingNaN
				return x
			}
			return y.isSignMinus ? -BigReal.π : BigReal.π
		} else if y.isZero {
			return x.isSignMinus ? BigReal.π : BigReal(0)
		}
		
		if x == y {
			return y.isSignMinus ? -BigReal(3) * BigReal.π / BigReal(4) : BigReal.π / BigReal(4)
		}
		
		if x == -y {
			return y.isSignMinus ? -BigReal.π / BigReal(4) : BigReal(3) * BigReal.π / BigReal(4)
		}
		return (y/x).atan()
	}
	
	func hypot(y:BigReal)->BigReal {
		let x = icopy()
		let yt = y.number
		x.raiseToIntPower(2)
		yt.raiseToIntPower(2)
		x.add(yt)
		x.sqrt()
		return BigReal(x)
	}
	
	func pow(y:BigReal)->BigReal {
		let x = icopy()
		x.raiseToPower(y.number)
		return BigReal(x)
	}
	
	// these ought to be static let
	// but give users a chance to overwrite it
	static let PI = BigReal(BigFloat(piWithRadix: BigReal.radix))
	static let π = PI
	static let E = BigReal.ONE.exp()
	static let e = E
	static let LN2 = BigReal.TWO.ln()
	static let LOG2E = BigReal.ONE / LN2
	static let LN10 = BigReal.TEN.ln()
	static let LOG10E = E.log()
	static let SQRT2 = BigReal.TWO.sqrt()
	static let SQRT1_2 = BigReal.ONE/SQRT2
	static let ZERO = BigReal(0)
	static let ONE = BigReal(1)
	static let TWO = BigReal(2)
	static let TEN = BigReal(10)
    static let epsilon = BigReal(0x1p-52)  // fix this

}

public func + (lhs: BigReal, rhs: BigReal) -> BigReal { return lhs.add(rhs) }
public func += (inout lhs: BigReal, rhs: BigReal) { lhs = lhs + rhs }
//public func + (lhs: BigReal, rhs: Double) -> BigReal { return lhs.add(BigReal(rhs)) }
//public func + (lhs: Double, rhs: BigReal) -> BigReal { return BigReal(lhs).add(rhs) }

public func - (lhs: BigReal, rhs: BigReal) -> BigReal { return lhs.sub(rhs) }
public func -= (inout lhs: BigReal, rhs: BigReal) { lhs = lhs - rhs }
//public func - (lhs: BigReal, rhs: Double) -> BigReal { return lhs.sub(BigReal(rhs)) }
//public func - (lhs: Double, rhs: BigReal) -> BigReal { return BigReal(lhs).sub(rhs) }

public func * (lhs: BigReal, rhs: BigReal) -> BigReal { return lhs.mul(rhs) }
public func *= (inout lhs: BigReal, rhs: BigReal) { lhs = lhs * rhs }
//public func * (lhs: BigReal, rhs: Double) -> BigReal { return lhs.mul(BigReal(rhs)) }
//public func * (lhs: Double, rhs: BigReal) -> BigReal { return BigReal(lhs).mul(rhs) }

public func / (lhs: BigReal, rhs: BigReal) -> BigReal { return lhs.div(rhs) }
public func /= (inout lhs: BigReal, rhs: BigReal) { lhs = lhs / rhs }
//public func / (lhs: BigReal, rhs: Double) -> BigReal { return lhs.div(BigReal(rhs)) }
//public func / (lhs: Double, rhs: BigReal) -> BigReal { return BigReal(lhs).div(rhs) }

//public func ** (lhs: BigReal, rhs: Int) -> BigReal { return lhs.ipower(rhs) }
//public func ** (lhs: Double, rhs: Int) -> BigReal { return BigReal(lhs).ipower(rhs) }
//public func ** (lhs: Int, rhs: Int) -> BigReal { return BigReal(lhs).ipower(rhs) }

public prefix func + (lhs: BigReal)->BigReal { return lhs }
public prefix func - (lhs: BigReal)->BigReal { return lhs.negate() }

public func == (lhs: BigReal, rhs: BigReal) -> Bool { return lhs.cmp(rhs) == .OrderedSame }
public func < (lhs: BigReal, rhs: BigReal) -> Bool { return lhs.cmp(rhs) == .OrderedAscending }



