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
    
    init(_ value: Int)    { self.init(value, radix:10) }
    init(_ value: UInt8)  { self.init(Int(value), radix:10) }
    init(_ value: Int8)   { self.init(Int(value), radix:10) }
    init(_ value: UInt16) { self.init(Int(value), radix:10) }
    init(_ value: Int16)  { self.init(Int(value), radix:10) }
    init(_ value: UInt32) { self.init(Int(value), radix:10) }
    init(_ value: Int32)  { self.init(Int(value), radix:10) }
    init(_ value: UInt64) { self.init(Int(value), radix:10) }
    init(_ value: Int64)  { self.init(Int(value), radix:10) }
    init(_ value: UInt)   { self.init(Int(value), radix:10) }
    init(_ value: Double) { self.init(value, radix:10) }
    init(_ value: Float)  { self.init(Double(value), radix:10) }
    
    var isSignMinus: Bool { return isNegative }
    var isNormal: Bool { return isValid }
    var isFinite: Bool { return isValid }
    var isSubnormal: Bool { return !isValid }
    var isInfinite: Bool { return description == "∞" }
    var isNaN: Bool { return !isValid }
    var isSignaling: Bool { return !isValid }
    var hashValue: Int {
        return 0  // TBD
    }
    
    func cos()->Real { return cosWithTrigMode(.radians) }
    func sin()->Real { return sinWithTrigMode(.radians) }
    
    /// self * 1.0i
    var i:Complex<Real>{ return Complex<Real>(0, self) }
}

typealias RComplex = Complex<Real>

extension Complex : IntegerLiteralConvertible, FloatLiteralConvertible {
    
    init(_ real: Double, _ imag: Double = 0) { self.init(T(real), T(imag)) }
    
    //
    // IntegerLiteralConvertible compliance
    //
    init (integerLiteral value: Int) { self.init(Double(value)) }
    
    //
    // FloatLiteralConvertible compliance
    //
    init (floatLiteral value: Double) { self.init(value) }
    
}