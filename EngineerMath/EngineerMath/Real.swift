//
//  Real.swift
//  EngineerMath
//
//  Created by Mike Griebling on 5 Aug 2015.
//  Copyright © 2015 Solinst Canada. All rights reserved.
//
//  Real is an arbitrary precision (fixed at compile-time) arbitrary radix floating
//  point number format. The entire functionality of the class is implemented in a
//  single file for simple inclusion in other projects.
//
//  Precision is defined by BF_num_values. It defines how many UInt32 are
//  used to hold the number. Though in reality, only half of each UInt32 is used. This
//  is so that when you multiply them together, there is room for the result  (16 bits
//  multiplied by 16 bits requires all 32 bits). If you really wanted to, you could
//  change this class so that BF_num_values was chosen at class initialisation time
//
//  Based on BigFloat.m from the Magic Number Machine
//  Created by Matt Gallagher on Sun Jan 06 2002.
//  Copyright © 2002-2003 Matt Gallagher. All rights reserved.
//

import Foundation

struct Real {

    // Basic constants defining the precision used by the class
    static let BF_num_values            =	16   // was 8
    static let BF_max_mantissa_length	=	(BF_num_values * 16 + 3)
    static let BF_max_exponent_length	=	sizeof(Int32)*8
    
    // Mode for trigonometric operations
    enum BFTrigMode { case BF_degrees, BF_radians, BF_gradians }
    
    typealias Digit = UInt32
    
    private var bf_array = [Digit](count: Real.BF_num_values, repeatedValue: 0)
    private var bf_exponent: Int32
    private var bf_user_point: Int32
    private var bf_is_negative: Bool
    
    private var bf_radix: UInt8
    private var bf_value_precision: UInt32
    private var bf_value_limit: UInt32
    private var bf_exponent_precision: UInt32
    
    var isValid: Bool
    
    // MARK: - Helper Functions
    
    //
    // Sets every value in a values array to zero
    //
    static func BF_ClearValuesArray(inout values: [Digit]) {
        // Set the value to zero
        values = [UInt32](count: values.count, repeatedValue: 0)
//        for i in 0..<values.count {
//            values[i] = 0
//        }
    }
    
    //
    // Scans a values array looking for any non-zero digits.
    //
    static func BF_ArrayIsNonZero(values: [Digit]) -> Bool {
        for i in 0..<values.count {
            if values[i] != 0 { return true }
        }
        
        return false
    }
    
    //
    // Copies the source values to the destination values.
    //
    static func BF_CopyValues(source: [Digit], inout destination: [Digit]) {
        // Do a basic copy of the values into the destination
        destination = source
//        for i in 0..<source.count {
//            destination[i] = source[i]
//        }
    }
    
    //
    // Adds a single UInt32 to an array of values.
    //
    static func BF_AddToMantissa(inout values: [Digit], var digit: Digit, limit: Digit) {
        for i in 0..<values.count {
            values[i] += digit
            digit      = values[i] / limit
            values[i] %= limit
        }
        
        values[values.count - 1] += digit * limit
    }
    
    //
    // Appends a single radix digit to the least significant end of the values array. Space
    // is made for the digit by multiplying through by the radix first.
    //
    static func BF_AppendDigitToMantissa(inout values: [Digit], var digit: Digit, radix: UInt8, limit: Digit) {
        // Multiply through by the bf_radix and add the digit
        for i in 0..<values.count {
            values[i] = (values[i] * UInt32(radix)) + digit
            digit	  = values[i] / limit
            values[i] = values[i] % limit
        }
        values[values.count - 1] += digit * limit
    }
    
    //
    // Chops a single digit off the end of the values array by dividing through by the radix.
    //
    static func BF_RemoveDigitFromMantissa(inout values: [Digit], radix: UInt8, limit: Digit) -> Digit  {
        // Truncate a digit by dividing through by the bf_radix
        var carryBits : Digit = 0
        
        for i in (0..<values.count).reverse() {
            values[i] = values[i] + (carryBits * limit)
            carryBits = values[i] % UInt32(radix)
            values[i] = values[i] / UInt32(radix)
        }
        
        return carryBits
    }
    
    //
    // Chops a single digit off the end of the values array by dividing through by the radix.
    // If the result is negative it says so.
    //
    static func BF_RemoveDigitFromMantissaAndFlagEmpty(inout values: [Digit], radix: UInt8, limit: Digit, inout isEmpty: Bool) -> Digit {
        // Truncate a digit by dividing through by the bf_radix
        var carryBits: Digit = 0
        var empty = true
        
        for i in (0..<values.count).reverse() {
            values[i] = values[i] + (carryBits * limit)
            carryBits = values[i] % UInt32(radix)
            values[i] = values[i] / UInt32(radix)
            if values[i] != 0 { empty = false }
        }
        
        isEmpty = empty
        return carryBits
    }
    
    //
    // Counts the number of digits after and including the most significant non-zero digit.
    //
    static func BF_NumDigitsInArray(inout values: [Digit], radix: UInt8, precision: Digit) -> Int {
        var digitsInNumber, valueNumber, digitNumber: Int
        
        // Trace through the number looking the the most significant non-zero digit
        digitsInNumber =  BF_num_values * Int(precision)
        valueNumber = BF_num_values
        var digitPower1: Int
        repeat {
            valueNumber--
            digitNumber = Int(precision - 1)
            let digitPower = Int(pow(Double(radix), Double(digitNumber)))
            while (Int(values[valueNumber]) / digitPower) % Int(radix) == 0 && digitNumber >= 0 {
                digitNumber--
                digitsInNumber--
            }
            digitPower1 = Int(pow(Double(radix), Double(digitNumber)))
        } while (Int(values[valueNumber]) / digitPower1) % Int(radix) == 0 && valueNumber > 0
        
        return digitsInNumber
    }
    
    //
    // Normalises the mantissas of two floating point numbers so that they can be added
    // subtracted or compared.
    //
    static func BF_NormaliseNumbers(inout thisNum: Real, inout otherNum: Real) {
        assert(otherNum.bf_radix == thisNum.bf_radix, "Numbers must have same radix before normalisation")
        
        var thisRoundingNum : Digit = 0
        var otherRoundingNum : Digit = 0
        var thisEmpty = false
        var otherEmpty = false
        
        thisNum.bf_exponent -= thisNum.bf_user_point
        thisNum.bf_user_point = 0
        otherNum.bf_exponent -= otherNum.bf_user_point
        otherNum.bf_user_point = 0
        
        // Normalise due to otherNum.bf_exponent being greater than bf_exponent
        if otherNum.bf_exponent > thisNum.bf_exponent {
            // start by normalising otherNum left
            while otherNum.bf_exponent > thisNum.bf_exponent &&
                  otherNum.bf_array[BF_num_values - 1] < (otherNum.bf_value_limit / Digit(otherNum.bf_radix))
            {
                BF_AppendDigitToMantissa(&otherNum.bf_array, digit: 0, radix: otherNum.bf_radix, limit: otherNum.bf_value_limit)
                otherNum.bf_exponent--
            }
            
            // then normalise this num to the right
            while otherNum.bf_exponent > thisNum.bf_exponent && !thisEmpty {
                thisRoundingNum = BF_RemoveDigitFromMantissaAndFlagEmpty(&thisNum.bf_array, radix: thisNum.bf_radix, limit: thisNum.bf_value_limit, isEmpty: &thisEmpty)
                thisNum.bf_exponent++
            }
        }
            
        // Normalise due to this bf_exponent being greater than otherNum->bf_exponent
        else if thisNum.bf_exponent > otherNum.bf_exponent {
            // start by normalising this num left
            while thisNum.bf_exponent > otherNum.bf_exponent &&
                  thisNum.bf_array[BF_num_values - 1] < (thisNum.bf_value_limit / Digit(thisNum.bf_radix))
            {
                BF_AppendDigitToMantissa(&thisNum.bf_array, digit: 0, radix: thisNum.bf_radix, limit: thisNum.bf_value_limit)
                thisNum.bf_exponent--
            }
            // then normalise otherNum to the right
            while thisNum.bf_exponent > otherNum.bf_exponent && !otherEmpty {
                otherRoundingNum = BF_RemoveDigitFromMantissaAndFlagEmpty(&otherNum.bf_array, radix: otherNum.bf_radix, limit: otherNum.bf_value_limit, isEmpty: &otherEmpty)
                otherNum.bf_exponent++
            }
        }
        
        // Apply a round to nearest on any truncated values
        if (!otherEmpty && Double(otherRoundingNum) >= (Double(thisNum.bf_radix) / 2.0)) {
            BF_AddToMantissa(&otherNum.bf_array, digit: 1, limit: otherNum.bf_value_limit)
        } else if (!thisEmpty && Double(thisRoundingNum) >= (Double(thisNum.bf_radix) / 2.0)) {
            BF_AddToMantissa(&thisNum.bf_array, digit: 1, limit: thisNum.bf_value_limit)
        }
        
        if thisEmpty && !otherEmpty {
            thisNum.bf_exponent = otherNum.bf_exponent
        } else if !thisEmpty && otherEmpty {
            otherNum.bf_exponent = thisNum.bf_exponent
        } else if thisEmpty && otherEmpty {
            otherNum.bf_exponent = 0
            thisNum.bf_exponent = 0
        }
    }
    

    // MARK: - Contructors
    init () {
        
    }
}