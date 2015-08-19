//
//  RComplex.swift
//  EngineerMath
//
//  Created by Mike Griebling on 18 Aug 2015.
//  Copyright © 2015 Solinst Canada. All rights reserved.
//

import Foundation

//
// Extension to support RealType protocol for Complex generic instance
//
extension Real : RealType {
    
    public init(_ value: Int)    { self.init(value, radix:10) }
    public init(_ value: UInt8)  { self.init(Int(value), radix:10) }
    public init(_ value: Int8)   { self.init(Int(value), radix:10) }
    public init(_ value: UInt16) { self.init(Int(value), radix:10) }
    public init(_ value: Int16)  { self.init(Int(value), radix:10) }
    public init(_ value: UInt32) { self.init(Int(value), radix:10) }
    public init(_ value: Int32)  { self.init(Int(value), radix:10) }
    public init(_ value: UInt64) { self.init(Int(value), radix:10) }
    public init(_ value: Int64)  { self.init(Int(value), radix:10) }
    public init(_ value: UInt)   { self.init(Int(value), radix:10) }
    public init(_ value: Double) { self.init(value, radix:10) }
    public init(_ value: Float)  { self.init(Double(value), radix:10) }
	public init(_ value: String) { self.init(value, radix:10) }
    
    public var isSignMinus: Bool { return isNegative }
    public var isNormal: Bool { return isValid }
    public var isFinite: Bool { return isValid }
    public var isSubnormal: Bool { return !isValid }
    public var isInfinite: Bool { return description == "∞" }
    public var isNaN: Bool { return !isValid }
    public var isSignaling: Bool { return !isValid }
    
    public func cos()->Real { return cosWithTrigMode(.radians) }
    public func sin()->Real { return sinWithTrigMode(.radians) }
    
    /// self * 1.0i
    var i: RComplex { return RComplex(0, self) }
}

typealias RComplex = Complex<Real>

extension Complex : IntegerLiteralConvertible, FloatLiteralConvertible, CustomStringConvertible, StringLiteralConvertible {
    
    init(_ real: Double, _ imag: Double = 0) { self.init(T(real), T(imag)) }
    init(_ string: String) { self.init(stringLiteral:string) }
    
    //
    // IntegerLiteralConvertible compliance
    //
    init (integerLiteral value: Int) { self.init(Double(value)) }
    
    //
    // FloatLiteralConvertible compliance
    //
    init (floatLiteral value: Double) { self.init(value) }
	
	//
	// StringLiteralConvertible compliance
	//
	typealias ExtendedGraphemeClusterLiteralType = StringLiteralType
	typealias UnicodeScalarLiteralType = Character
	init(extendedGraphemeClusterLiteral value: ExtendedGraphemeClusterLiteralType) { self.init(stringLiteral: value) }
	init(unicodeScalarLiteral value: UnicodeScalarLiteralType) { self.init(stringLiteral: "\(value)") }
	init(stringLiteral s: String) {
        let imaginary = "i"
        var vs = s.stringByReplacingOccurrencesOfString(" ", withString: "").lowercaseString  // remove all spaces & make lowercase
        var number = ""
        var inumber = ""
        
        func processNumber() {
            if let _ = vs.rangeOfString(imaginary) {
                inumber = number + vs 	// transfer the sign
                number = ""				// clear the real part
            } else {
                number += vs			// copy the number
            }
        }

        self.init()
		if !vs.isEmpty {
			// break apart the string into real and imaginary pieces
			let signChars = NSCharacterSet(charactersInString: "+-")
			let exponent = "e"
			var ch = vs[vs.startIndex]
			
			// remove leading sign -- if any
			if signChars.characterIsMember(ch.toUnichar()) { number.append(ch); ch = vs.removeAtIndex(vs.startIndex) }
			if var range = vs.rangeOfCharacterFromSet(signChars) {
				// check if this is an exponent
				if let expRange = vs.rangeOfString(exponent) where expRange.startIndex == range.startIndex.predecessor() {
					// search beyond the exponent
					range = range.startIndex.successor()...vs.endIndex
					if let range = vs.rangeOfCharacterFromSet(signChars, options: [], range: range) {
						// This is likely the start of the second number
						number += vs.substringToIndex(range.startIndex)
						inumber = vs.substringFromIndex(range.startIndex)
					} else {
						// Only one number exists
                        processNumber()
					}
				} else {
					// This is the start of the second number
					number += vs.substringToIndex(range.startIndex)
					inumber = vs.substringFromIndex(range.startIndex)
				}
			} else {
				// only one number exists
                processNumber()
			}
			re = T(number)!
			let iPresent = !inumber.isEmpty
			inumber = inumber.stringByReplacingOccurrencesOfString(imaginary, withString: "") // remove the "i"
			
			// account for solitary "i"
			if iPresent {
				if inumber.isEmpty { inumber = "1" }
				else if inumber == "+" || inumber == "-" { inumber += "1" }
			}
			im = T(inumber)!
		}
	}
	
	//
	// CustomStringConvertible compliance
	//
	var description: String {
		let isOne = im.abs == 1
		let plus = im.isSignMinus ? isOne ? "-" : "" : "+"
		let imag = im.isZero ? "" : isOne ? "\(plus)i" : "\(plus)\(im)i"
		return "\(re)\(imag)"
	}
	
}

