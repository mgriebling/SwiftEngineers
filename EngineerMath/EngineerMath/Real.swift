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

// Comparable compliance
public func == (lhs: Real, rhs: Real) -> Bool { return lhs.compareWith(rhs) == .OrderedSame }
public func < (lhs: Real, rhs: Real) -> Bool { return lhs.compareWith(rhs) == .OrderedAscending }

prefix func + (z:Real) -> Real { return z }
public func + (lhs: Real, rhs: Real) -> Real { return lhs.add(rhs) }
public func += (inout lhs: Real, rhs: Real) { lhs = lhs + rhs }
prefix func - (z:Real) -> Real { return z.negate() }
public func - (lhs: Real, rhs: Real) -> Real { return lhs.subtract(rhs) }
public func -= (inout lhs: Real, rhs: Real) { lhs = lhs - rhs }
public func * (lhs: Real, rhs: Real) -> Real { return lhs.multiplyBy(rhs) }
public func *= (inout lhs: Real, rhs: Real) { lhs = lhs * rhs }
public func / (lhs: Real, rhs: Real) -> Real { return lhs.divideBy(rhs) }
public func /= (inout lhs: Real, rhs: Real) { lhs = lhs / rhs }
public func % (lhs: Real, rhs: Real) -> Real { return lhs.moduloBy(rhs) }
public func %= (inout lhs: Real, rhs: Real) { lhs = lhs % rhs }

public struct Real : CustomStringConvertible, Comparable {

    // Basic constants defining the precision used by the class
    static let BF_num_values            =	16   // number of digit limbs
	static let BF_digit_bits			=   16   // bits per digit limb
	static let BF_max_radix				=   1 << BF_digit_bits
    static let BF_max_mantissa_length	=	BF_num_values * BF_digit_bits + 3
    static let BF_max_exponent_length	=	sizeof(Int32)*8
	static let BF_max_exponent			=   Int(Int16.max)
	
	// A string containing the unichar digits 0 to 9 and onwards
	static let BF_digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    
    // Mode for trigonometric operations
    public enum BFTrigMode { case BF_degrees, BF_radians, BF_gradians }
    
    typealias Digit = UInt32
    
    private var bf_array = [Digit](count: Real.BF_num_values, repeatedValue: 0)
    private var bf_exponent: Int = 0
    private var bf_user_point: Int = 0
    private var bf_is_negative: Bool = false
    
    private var bf_radix: Int = 10
    private var bf_value_precision: Digit = 0
    private var bf_value_limit: Digit = 0
	private var bf_is_valid: Bool = false
		
    // MARK: - Helper Functions
    
    //
    // Sets every value in a values array to zero
    //
    static func BF_ClearValuesArray(inout values: [Digit]) {
        // Set the value to zero
        values = [Digit](count: values.count, repeatedValue: 0)
    }
    
    //
    // Scans a values array looking for any non-zero digits.
    //
    static func BF_ArrayIsNonZero(values: [Digit], start: Int = 0) -> Bool {
        for i in start..<values.count {
            if values[i] != 0 { return true }
        }
        
        return false
    }
    
    //
    // Copies the source values to the destination values.
    //
    static func BF_CopyValues(source: [Digit], inout destination: [Digit], sstart: Int = 0, dstart: Int = 0) {
        // Do a basic copy of the values into the destination
        for i in 0..<BF_num_values {
            destination[i+dstart] = source[i+sstart]
        }
    }
    
    static func BF_AssignValues(inout destination: [Digit], source: [Digit], sstart: Int = 0, dstart: Int = 0) {
        BF_CopyValues(source, destination: &destination, sstart: sstart, dstart: dstart)
    }
    
    //
    // Adds a single UInt32 to an array of values.
    //
    static func BF_AddToMantissa(inout values: [Digit], var digit: Digit, limit: Digit, start: Int = 0, scale: Int = 1) {
        for i in start..<start+BF_num_values*scale {
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
    static func BF_AppendDigitToMantissa(inout values: [Digit], var digit: Digit, radix: Int, limit: Digit, scale: Int = 1) {
        // Multiply through by the bf_radix and add the digit
        for i in 0..<BF_num_values * scale {
            values[i] = (values[i] * UInt32(radix)) + digit
            digit	  = values[i] / limit
            values[i] = values[i] % limit
        }
        values[values.count - 1] += digit * limit
    }
    
    //
    // Chops a single digit off the end of the values array by dividing through by the radix.
    //
    static func BF_RemoveDigitFromMantissa(inout values: [Digit], radix: Int, limit: Digit, start: Int = 0, scale: Int = 1) -> Digit  {
        // Truncate a digit by dividing through by the bf_radix
        var carryBits : Digit = 0
        
        for i in (start..<start+BF_num_values*scale).reverse() {
            values[i] = values[i] + (carryBits * limit)
            carryBits = values[i] % Digit(radix)
            values[i] = values[i] / Digit(radix)
        }
        
        return carryBits
    }
    
    //
    // Chops a single digit off the end of the values array by dividing through by the radix.
    // If the result is negative it says so.
    //
    static func BF_RemoveDigitFromMantissaAndFlagEmpty(inout values: [Digit], radix: Int, limit: Digit, inout isEmpty: Bool) -> Digit {
        // Truncate a digit by dividing through by the bf_radix
        var carryBits: Digit = 0
        var empty = true
        
        for i in (0..<values.count).reverse() {
            values[i] = values[i] + (carryBits * limit)
            carryBits = values[i] % Digit(radix)
            values[i] = values[i] / Digit(radix)
            if values[i] != 0 { empty = false }
        }
        
        isEmpty = empty
        return carryBits
    }
    
    //
    // Counts the number of digits after and including the most significant non-zero digit.
    //
    static func BF_NumDigitsInArray(values: [Digit], radix: Int, precision: Digit) -> Int {
        var digitsInNumber, valueNumber, digitNumber: Int
        
        // Trace through the number looking the the most significant non-zero digit
        digitsInNumber =  BF_num_values * Int(precision)
        valueNumber = BF_num_values
        repeat {
            valueNumber--
            digitNumber = Int(precision - 1)
            while Int(Double(values[valueNumber]) / Real.pow(radix, digitNumber)) % radix == 0 && digitNumber >= 0 {
                digitNumber--
                digitsInNumber--
            }
        } while Int(Double(values[valueNumber]) / Real.pow(radix, digitNumber)) % radix == 0 && valueNumber > 0
        
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
	
	// MARK: - Private utility functions
	
	//
	// Allows the elements of a BigFloat to be safely set.
	//
	private mutating func setElements(var radix: Int, negative isNegative:Bool, exp exponent:Int, valid isValid:Bool, var userPoint:Int) {
		// Set everything
		bf_exponent = exponent
		bf_is_negative = isNegative
		bf_is_valid = isValid
		
		// Set the bf_radix (if it is valid)
		if radix < 2 || radix > 36 { radix = 10 }
		bf_radix = radix
		
		let rradix = Double(radix)
		let rmax = Double(Real.BF_max_radix)
		bf_value_precision = Digit(log(rmax) / log(rradix))
		bf_value_limit = Real.pow(radix, Int(bf_value_precision))
		
		// Apply the decimal point
		if userPoint > Int(bf_value_precision * Digit(Real.BF_num_values) - 1) {
			userPoint = Int(bf_value_precision * Digit(Real.BF_num_values) - 1)
		}
		bf_user_point = userPoint
	}
	
	//
	// Puts a fractional point in a number according to typical expected behaviour.
	//
	private mutating func createUserPoint() {
		if isZero {
			bf_exponent = 0
			bf_user_point = 0
			return
		}
		
		// Extract a user decimal point (because 45.67 is prettier than 4567e-2)
		if bf_exponent < 0 {
			if Digit(-bf_exponent) > bf_value_precision * Digit(Real.BF_num_values) {
				bf_exponent += Int32(bf_value_precision * Digit(Real.BF_num_values)) - 1
				bf_user_point = Int(bf_value_precision * Digit(Real.BF_num_values)) - 1
			} else {
				bf_user_point = -bf_exponent
				bf_exponent = 0
			}
		}
		
		// Standard check on the exponent
		if Swift.abs(bf_exponent) > Real.BF_max_exponent {
			bf_is_valid = false
		}
	}
    
    public func toRadiansFrom(mode: BFTrigMode) -> Real {
        var result = self
        if mode != .BF_radians {
            if mode == .BF_degrees {
                let oneEighty = Real(180, radix: bf_radix)
                let threeSixty = Real(360, radix: bf_radix)
                result = (self / oneEighty) % threeSixty
            } else if mode == .BF_gradians {
                let twoHundred = Real(200, radix: bf_radix)
                let fourHundred = Real(400, radix: bf_radix)
                result = (self / twoHundred) % fourHundred
            }
            return result * π
        } else {
            let two_pi = two * π
            return self % two_pi
        }
    }
    
    public func radiansToMode(mode: BFTrigMode) -> Real {
        var result = self
        if mode != .BF_radians {
            if mode == .BF_degrees {
                let oneEighty = Real(180, radix: bf_radix)
                result = self * oneEighty
            } else if mode == .BF_gradians {
                let twoHundred = Real(200, radix: bf_radix)
                result = self * twoHundred
            }
            return result / π
        }
        return self
    }
    
    private func preComplement(complement: Int) -> Real {
        if complement != 0 {
            if self.isNegative {
                let complementHalf = UInt64(1) << UInt64(complement - 1)
                let complementNumberHalf = Real(mantissa:complementHalf, exponent:0, isNegative:false, radix:bf_radix, userPointAt:0)
                let complementNumberFull = complementNumberHalf * two
                return complementNumberFull + self
            }
        }
        return self
    }
    
    private func postComplement(complement: Int) -> Real {
        if complement != 0 {
            let complementHalf = UInt64(1) << UInt64(complement - 1)
            let complementNumberHalf = Real(mantissa:complementHalf, exponent:0, isNegative:false, radix:bf_radix, userPointAt:0)
            let complementNumberFull = complementNumberHalf * two

            if self >= complementNumberHalf {
                return zero - (complementNumberFull - self)
            }
        }
        return self
    }
    
    private static func pow (x: Int, _ power: Int) -> Double {
        return Foundation.pow(Double(x), Double(power))
    }
    
    private static func pow (x: Int, _ power: Int) -> Digit {
        let power: Double = pow(x, power)
        return Digit(power)
    }
    
    //
    // receiver = op(receiver, num)
    //
    private func opWith(num: Real, usingComplement complement: Int, andOp op: (Digit, Digit) -> Digit) -> Real {
        if !bf_is_valid { return self }
        if !num.isValid { return num }
        
        var thisNum = self.preComplement(complement)
        var otherNum = num.preComplement(complement)
        
        // Convert to a radix that is a power of 2
        let old_radix = bf_radix
        if bf_radix != 2 && bf_radix != 4 && bf_radix != 8 && bf_radix != 16 && bf_radix != 32 {
            // Convert to the largest possible binary compatible radix
            thisNum = thisNum.convertToRadix(32)
        }
        otherNum = otherNum.convertToRadix(thisNum.bf_radix)
        
        // look for the first digit
        var digit = Real.BF_num_values * Int(thisNum.bf_value_precision) - 1
        var ind = digit /  Int(thisNum.bf_value_precision)
        var offset: Digit = Real.pow(thisNum.bf_radix, digit % Int(thisNum.bf_value_precision))
        while offset != 0 && thisNum.bf_array[ind] / offset == 0 && otherNum.bf_array[ind] / offset == 0 && digit >= 0 {
            digit--
            ind = digit / Int(thisNum.bf_value_precision)
            offset = Real.pow(bf_radix, digit % Int(thisNum.bf_value_precision))
        }
        
        // apply a binary op to each digit
        while offset != 0 && digit >= 0 {
            var this_digit = (thisNum.bf_array[ind] / offset) % Digit(thisNum.bf_radix)
            let that_digit = (otherNum.bf_array[ind] / offset) % Digit(thisNum.bf_radix)
            thisNum.bf_array[ind] -= this_digit * Digit(offset)
            this_digit = op(this_digit, that_digit) % Digit(thisNum.bf_radix)
            thisNum.bf_array[ind] += this_digit * offset
            
            digit--
            ind = digit / Int(thisNum.bf_value_precision)
            offset = Real.pow(thisNum.bf_radix, digit % Int(thisNum.bf_value_precision))
        }
        
        // Restore the radix
        thisNum = thisNum.convertToRadix(old_radix)
        return thisNum.postComplement(complement)
    }


    // MARK: - Contructors

	//
	// Hey look, its the default constructor. Bet you've never seen one of these before.
	// By default you get a base 10 zero.
	//
	public init() {
		Real.BF_ClearValuesArray(&bf_array)
		setElements(10, negative:false, exp:0, valid:true, userPoint:0)
	}

	//
	// Allows fairly explicit contruction of a Real
	//
	public init(var mantissa: UInt64, exponent exp: Int, isNegative flag: Bool, radix newRadix: Int, userPointAt pointLocation: Int) {
		Real.BF_ClearValuesArray(&bf_array)
		setElements(newRadix, negative:flag, exp:exp, valid:true, userPoint:pointLocation)
		
		// Set the values
		let limit = UInt64(bf_value_limit)
		bf_array[0] = Digit(mantissa % limit); mantissa /= limit
		bf_array[1] = Digit(mantissa % limit); mantissa /= limit
		if Real.BF_num_values > 2 {
			bf_array[2] = Digit(mantissa % limit); mantissa /= limit
			if Real.BF_num_values > 3 {
				bf_array[3] = Digit(mantissa % limit); mantissa /= limit
				if Real.BF_num_values > 4 {
					bf_array[4] = Digit(mantissa % limit); mantissa /= limit
					if Real.BF_num_values > 5 {
						bf_array[5] = Digit(mantissa % limit)
					}
				}
			}
		}
	}

	//
	// The most common constructor. Simple and delicious.
	//
	public init (var _ newValue: Int, radix newRadix: Int = 10) {
		let negative = newValue < 0
		if negative { newValue = -newValue }
		
		self.init(mantissa: UInt64(newValue), exponent: 0, isNegative: negative, radix: newRadix, userPointAt: 0)
	}
	
	//
	// Also good but not as fast as initWithInt.
	//
	public init(var _ newValue: Double, radix newRadix: Int = 10) {
		var mantissa : UInt64 = 0
		var newExponent: Int
		var numDigits = 0
		var negative = false
		
		// Shortcut
		if newValue == 0.0 {
			self.init(0, radix:newRadix); return
		}
		
		// Determine what the bf_value_precision would be for this bf_radix
		let radixValuePrecision = Digit(log(Double(Real.BF_max_radix)) / log(Double(newRadix)))
		
		// Determine the sign
		if newValue < 0 {
			negative = true
			newValue = -newValue
		}
		
		// Get the base bf_radix exponent
		let doubleExponent = log(newValue) / log(Double(newRadix))
		if doubleExponent < 0 {
			newExponent = Int(floor(doubleExponent))
		} else {
			newExponent = Int(ceil(doubleExponent))
		}
		
		// Remove the exponent from the newValue
		newValue /= Real.pow(newRadix, newExponent)
		if newValue.isNaN {
			// Generate an NaN and return
			self.init(0, radix: newRadix)
			bf_is_valid = false
			return
		}
		
		// Get the digits out one at a time, up to the max precision for a double's mantissa
		var intPart = 0.0
		var fracPart: Double
		var nextDigit: Int
		for i in 0..<Int(Double(Int(radixValuePrecision) * sizeof(Double)/sizeof(UInt16)) * 0.8) {
			// The next digit should be the only thing left of the decimal point
			fracPart = modf(newValue, &intPart)
			nextDigit = Int(intPart)
			
			// Only add the digit if it is non-zero
			if nextDigit != 0 {
				// Guard against overflow
				if UInt64.max / UInt64(newRadix) >= mantissa {
                    let power: Double = Real.pow(newRadix, i - numDigits + 1)
					mantissa = mantissa * UInt64(power) + UInt64(nextDigit)
				}
				
				numDigits = i + 1
			}
			
			// Shift the next digit into place
			newValue = fracPart * Double(newRadix)
		}
		fracPart = modf(newValue, &intPart)
		if newValue > Double(newRadix / 2) && UInt64.max > mantissa {
			mantissa++
			while (mantissa % UInt64(newRadix) == 0 && numDigits > 1) {
				mantissa /= UInt64(newRadix)
				numDigits--
			}
		}
		
		// Now adjust the exponent into its correct spot
		newExponent -= (numDigits - 1);
		
		// Create the big float and return it
		self.init(mantissa:mantissa, exponent:newExponent, isNegative:negative, radix:newRadix, userPointAt:0)
		
		// Create a user point.
		createUserPoint()
		if Int(bf_user_point) >= numDigits {
			bf_exponent -= Int(bf_user_point) - numDigits + 1
			bf_user_point -= bf_user_point - numDigits + 1
		}
	}
	
	//
	// The most requested constructor for those numbers that don't fit in 19 digits.
	// BTW: I'll go with the Swift convention and use a 'p' exponent for radices other than 10.
	//
	public init(_ newValue: String, radix newRadix: Int = 10) {
		
		func getCharFromString(inout string: String) -> Character {
			if !string.isEmpty {
				let ch = string.removeAtIndex(string.startIndex)
				return ch
			}
			return "\0"
		}
		
		// Mike - created this primarily for the constants
		let separators = newRadix == 10 ? "eE" : "pP"
		let index = advance(Real.BF_digits.startIndex, Int(newRadix))
		let digits = Real.BF_digits.substringToIndex(index)
		let validDigits = NSCharacterSet(charactersInString: digits)
		let signChars = NSCharacterSet(charactersInString:"+-")
		var userPoint = 0
		
        self.init(0, radix: newRadix)
		let components = newValue.componentsSeparatedByCharactersInSet(NSCharacterSet(charactersInString:separators))
		var mantissa = components.first!.stringByTrimmingCharactersInSet(NSCharacterSet.whitespaceAndNewlineCharacterSet())
		var ch = getCharFromString(&mantissa)
		
		// extract the sign
		if signChars.characterIsMember(ch.toUnichar()) { appendDigit(ch, useComplement:0); ch = getCharFromString(&mantissa) }
		
		// extract the mantissa
		while validDigits.characterIsMember(ch.toUnichar()) {
			appendDigit(ch, useComplement:0)
			ch = getCharFromString(&mantissa)
		}
		
		// extract the fraction
		if ch == "." {
			ch = getCharFromString(&mantissa)
			while validDigits.characterIsMember(ch.toUnichar()) {
				appendDigit(ch, useComplement:0)
				ch = getCharFromString(&mantissa)
				userPoint++
			}
		}
		
		// extract the exponent
		if components.count > 1 {
			var expString = components[1]
			
			// add exponent to the number
			var expNegative = false
			if !expString.isEmpty {
				ch = getCharFromString(&expString)
				if signChars.characterIsMember(ch.toUnichar()) {
					expNegative = ch == "-"
					ch = getCharFromString(&expString)
				}
				while validDigits.characterIsMember(ch.toUnichar()) {
					appendExpDigit(ch)
					ch = getCharFromString(&expString)
				}
				if (expNegative) { appendExpDigit("-") }
			}
		}
		setUserPoint(userPoint)
	}
	
	// MARK: - Public utility functions
    
    public var isInteger: Bool {
        let tmp = self.abs().fractionalPart
        return tmp.isZero
    }
    
    //
    // Calculate π for the current bf_radix and cache it in the array
    // Uses the following iterative method to calculate π (quartically convergent):
    //
    //	Initial: Set y = sqrt(sqrt(2)-1), c = 0 and p = sqrt(2) - 1
    //	Loop: Set c = c+1
    // 			Set a = (1-y^4)^(1/4)
    //			Set y = (1-a)/(1+a)
    //			Set p = p(1+y)^4-y(1+y+y^2)sqrt(2)4^(c+1)
    //	π = 1/p
    //
    //	(for those of you playing at home... this is the Ramanujan II formula for π)
    //
    func calculatePi() -> Real {
        // Setup the initial conditions
        let two_sqrt	= two.sqrt()
        let quarter 	= Real(0.25, radix: bf_radix)
        var p 			= two_sqrt - one
        var y			= p.sqrt()
		var v			= one
		var x			= one
		var w			= one
        
        // Just allocate everything that is initially undefined
        var prevIteration = one
        
        // Do the loopy bit
        while p != prevIteration || !p.isValid {
            prevIteration = p
            
            // a = (1-y^4)^(1/4)
            let a = (one - y.raiseToIntPower(4)).raiseToPower(quarter)
            
            // y = (1-a)/(1+a)
            y = (one - a)/(one + a)
            
            // p = p(1+y)^4-y(1+y+y^2)sqrt(2)4^(c+1)
			x = y+one
            w = (y*y+x)*y
			x = x.raiseToIntPower(4)
            v *= four
            w *= v * two_sqrt
            
            if x.isValid && w.isValid {
                p *= x
                p -= w
            }
        }
        
        // pi_array is retained permanently (until the program quits)
        Real.piArray[Int(bf_radix)] = p.inverse
        return Real.piArray[Int(bf_radix)]!
    }
	
	//
	// I really don't like this but it is convenient.  It seems weird
	// to have a number act like a factory.
    private static var piArray = [Real?](count: 36, repeatedValue: nil)
    public var pi: Real {
        if let mpi = Real.piArray[Int(bf_radix)] {
            return mpi
        } else {
            return self.calculatePi()
        }
    }
    public var π: Real { return pi }
	public var zero: Real { return Real(0, radix:bf_radix) }
	public var one: Real { return Real(1, radix:bf_radix) }
	public var two: Real { return Real(2, radix:bf_radix) }
	public var four: Real { return Real(4, radix:bf_radix) }
    
    //
    // Reports what the current fractional point's location is.
    //
    public var userPoint: Int { return Int(bf_user_point) }
    
    //
    // Returns the number of digits in the number.
    //
    public var mantissaLength: Int {
        return Int(Real.BF_NumDigitsInArray(bf_array, radix: bf_radix, precision: Digit(bf_value_precision)))
    }
    
    //
    // Returns the radix of the current number
    //
    public var radix : Int { return Int(bf_radix) }
    
    //
    // Returns whether or not this number is valid (overflow or divide by zero make numbers
    // invalid).
    //
    public var isValid: Bool { return bf_is_valid }
    
    //
    // Returns the sign of the current number.
    //
    public var isNegative: Bool { return bf_is_negative }
    
    //
    // Returns the presence of an exponent.
    //
    public var hasExponent: Bool { return bf_exponent != 0 }
    
    //
    // True if the number is empty.
    //
    public var isZero: Bool {
        return !Real.BF_ArrayIsNonZero(bf_array)
    }
    
    //
    // Sets the sign of the number to positive.
    //
    public func abs() -> Real {
        var temp = self
        temp.bf_is_negative = false
        return temp
    }
    
    //
    // Sets the sign of the number to positive.
    //
    public func negate() -> Real {
        var temp = self
        temp.bf_is_negative = !bf_is_negative
        return temp
    }
	
	//
	// Appends a new digit to this BigFloat. As though the next digit had been typed
	// into the calculator. 2's complement numbers have some weird stuff that needs
	// to be worked through once the digit is appended.
	//
	public mutating func appendDigit(digit: Character, useComplement complement:Int) -> Bool {
		var values = [Digit](count: bf_array.count, repeatedValue: 0)
		
		if digit == "-" {
			bf_is_negative = (bf_is_negative == false) ? true : false
		} else if (digit >= "0" && digit <= "9") || (digit >= "A" && digit <= "Z") { // append a regular digit
			// Do nothing if overflow could occur
			if bf_array[Real.BF_num_values - 1] >= (bf_value_limit / Digit(bf_radix)) {
				return false
			}
			
			Real.BF_CopyValues(bf_array, destination: &values)
			
			// Multiply through by the bf_radix and add the digit
			let idigit: Digit
			let aChar : Character = "A"
			if digit <= "9" { idigit = Digit(String(digit))! }
			else { idigit = Digit(digit.unicodeValue() - aChar.unicodeValue() + 10) }
			Real.BF_AppendDigitToMantissa(&values, digit: idigit, radix: bf_radix, limit: bf_value_limit)
			
			if complement != 0 {
				var complementNumberFull: Real
				var complementNumberHalf: Real
				var mantissaNumber: Real
				let complementHalf = UInt64(1) << UInt64(complement - 1)
				let complementFull = UInt64(1) << UInt64(complement)
				
				complementNumberHalf = Real(mantissa: complementHalf, exponent: 0, isNegative: false, radix: bf_radix, userPointAt: 0)
				
				if complement == 64 {
					complementNumberFull = complementNumberHalf.multiplyBy(two)
				} else {
					complementNumberFull = Real(mantissa: complementFull, exponent: 0, isNegative: false, radix: bf_radix, userPointAt: 0)
				}
				
				mantissaNumber = complementNumberHalf
				Real.BF_AssignValues(&mantissaNumber.bf_array, source: values)
				
				if !bf_is_negative {
					let relative = mantissaNumber.compareWith(complementNumberHalf)
					
					if (relative == .OrderedDescending || relative == .OrderedSame) {
						if mantissaNumber.compareWith(complementNumberFull) == .OrderedAscending {
							complementNumberFull = complementNumberFull.subtract(mantissaNumber)
							Real.BF_AssignValues(&bf_array, source: complementNumberFull.bf_array);
							if (bf_user_point != 0) {
								bf_user_point++
							}
							bf_is_negative = true
							return true
						}
						
						// Overflow, don't apply digit
						return false
					}
				} else {
					// Overflow, don't apply digit
					return false
				}
				
			}
			
			Real.BF_AssignValues(&bf_array, source: values)
			
			// Move the decimal point along with the digits
			if bf_user_point != 0 {
				bf_user_point++
			}
		}
		
		return true
	}
	
	//
	// Puts another digit on the exponent
	//
	public mutating func appendExpDigit (digit: Character) {
		// Change the sign when '+/-' is pressed
		if digit == "-" {
			bf_exponent = -bf_exponent
			return
		}
		
		// Do the appending stuff
		if digit >= "0" && digit <= "9" {
			// Do nothing if overflow could occur
            let power: Double = Real.pow(bf_radix, Int(bf_value_precision))
			if Swift.abs(bf_exponent) > Int(power / Double(bf_radix)) {
				return
			}
			
			bf_exponent = bf_exponent * bf_radix + Int(String(digit))!
		}
	}
	
	//
	// Puts a fractional point into the number
	//
	public mutating func setUserPoint(pointLocation: Int) {
		setElements(bf_radix, negative:bf_is_negative, exp:bf_exponent, valid:isValid, userPoint:pointLocation)
	}
    
    public func convertToRadix(newRadix: Int) -> Real {
        // Get a copy of the relevant stuff
        var values = self
        
        // Check for a valid new radix
        if (bf_radix == newRadix || newRadix < 2 || newRadix > 36) {
            return values
        }
        
        // ignore invalid numbers
        if !bf_is_valid {
            values.bf_radix = newRadix
            return values
        }
        
        // Apply the user's decimal point
        let exponent = bf_exponent - Int(bf_user_point)
        values.bf_user_point = 0

        // Adjust the precision related elements
        let rradix = Double(Real.BF_max_radix)
        values.bf_radix = newRadix
        values.bf_exponent = 0
//        values.bf_exponent_precision =  UInt32(log(rradix) / log(Double(values.bf_radix)))
        values.bf_value_precision = Digit(log(rradix) / log(Double(values.bf_radix)))
        values.bf_value_limit = Real.pow(newRadix, Int(values.bf_value_precision))
        
        // Clear the working space
        var reversed = [Digit](count:Real.BF_max_radix*2, repeatedValue:0)
        var result = [Digit](count:Real.BF_max_radix*2, repeatedValue:0)
        
        // Re-encode the mantissa
        for _ in 0..<(Int(values.bf_value_precision) * Real.BF_num_values * 2) {
            // Remove new digits from the old number by integer dividing by the new radix
            let carryBits = Real.BF_RemoveDigitFromMantissa(&values.bf_array, radix: newRadix, limit: bf_value_limit)
            
            // Put all the digits in the new number
            Real.BF_AppendDigitToMantissa(&reversed, digit: carryBits, radix: newRadix, limit: values.bf_value_limit, scale:2)
        }
        
        // Which is fine, except that all the digits are now reversed
        for _ in 0..<(Int(values.bf_value_precision) * Real.BF_num_values * 2) {
            // Take out backwards
            let carryBits = Real.BF_RemoveDigitFromMantissa(&reversed, radix: newRadix, limit: values.bf_value_limit, scale:2)
            
            // And put in forwards
            Real.BF_AppendDigitToMantissa(&result, digit: carryBits, radix: newRadix, limit: values.bf_value_limit, scale:2)
        }
        
        // if result is too big, truncate until it fits into the allowed space
        while result[Real.BF_num_values] > 0 {
            Real.BF_RemoveDigitFromMantissa(&result, radix: newRadix, limit: values.bf_value_limit, scale:2)
            values.bf_exponent++
        }
        
        // Create a Real with bf_radix = newRadix and value = oldRadix
        var exponentNum = Real(bf_radix, radix: newRadix)
        
        // Raise the BigFloat to the old exponent power
        exponentNum = exponentNum.raiseToIntPower(exponent)
        
        // Set the values and elements of this number
        Real.BF_AssignValues(&values.bf_array, source: result)
        
        // multiply this number by the BigFloat
        return values.multiplyBy(exponentNum)
    }
	
	// MARK: - Arithmetic Functions
    
    //
    // Takes the receiver to the nth power.
    //
    public func raiseToIntPower(n: Int) -> Real {
        var Z = zero
        var N = Swift.abs(n)
        var Y = one
        let isNegativePower = n < 0
        
        if !bf_is_valid { return self }
        
        if isZero {
            // Zero raised to anything except zero is zero (provided exponent is valid)
            if n == 0 { return Y }
            return self
        }
        
        Z = self
        while true {
            let t = N & 1; N /= 2
            if t != 0 {
                Y = Y.multiplyBy(Z)
            }
            if N == 0 { break }
            Z = Z.multiplyBy(Z)
        }
        if isNegativePower {
            return Y.inverse
        }
        return Y
    }
	
	//
	// If I have one apple and you give me another apple, how many apples do I have.
	//
	public func add(num: Real) -> Real {
		var thisNum = self
		var otherNum = num
		
		if num.radix != Int(bf_radix) {
			otherNum = num.convertToRadix(bf_radix)
		}
		
		// ignore invalid numbers
		if !otherNum.bf_is_valid || !thisNum.bf_is_valid {
			thisNum.bf_is_valid = false
			return thisNum
		}
		
		// Handle differences in sign by calling subtraction instead
		if otherNum.bf_is_negative != thisNum.bf_is_negative {
			thisNum.bf_is_negative = !bf_is_negative
			thisNum = thisNum.subtract(num)
			
			if !thisNum.isZero {
				thisNum.bf_is_negative = !thisNum.bf_is_negative
			}
			
			return thisNum
		}
		
		Real.BF_NormaliseNumbers(&thisNum, otherNum: &otherNum)
		
		// We can finally do the addition at this point (yay!)
		var carryBits : Digit = 0
		for i in 0..<Real.BF_num_values {
			thisNum.bf_array[i] = thisNum.bf_array[i] + otherNum.bf_array[i] + carryBits
			carryBits = thisNum.bf_array[i] / thisNum.bf_value_limit
			thisNum.bf_array[i] %= thisNum.bf_value_limit
		}
		
		// If we have exceeded the maximum precision, reel it back in
		if carryBits != 0 {
			thisNum.bf_array[Real.BF_num_values - 1] += carryBits * thisNum.bf_value_limit
			
			carryBits = Real.BF_RemoveDigitFromMantissa(&thisNum.bf_array, radix: thisNum.bf_radix, limit: thisNum.bf_value_limit)
			thisNum.bf_exponent++
		}
		
		// Apply round to nearest
		if (Double(carryBits) >= (Double(thisNum.bf_radix) / 2.0)) {
			Real.BF_AddToMantissa(&thisNum.bf_array, digit: 1, limit: thisNum.bf_value_limit)
			
			// If by shear fluke that cause the top digit to overflow, then shift back by one digit
			if thisNum.bf_array[Real.BF_num_values - 1] > thisNum.bf_value_limit {
				Real.BF_RemoveDigitFromMantissa(&thisNum.bf_array, radix: thisNum.bf_radix, limit: thisNum.bf_value_limit)
				thisNum.bf_exponent++
			}
		}
		
		// Create a user point, store all the values back in the class and we're done
		thisNum.createUserPoint()
		return thisNum
	}

	
	//
	// I have two apples (one of them used to be yours). I eat yours. How many apples do
	// I have now?.
	//
	public func subtract(num: Real) -> Real {
		var thisNum = self
		var otherNum = num
		
		if num.radix != Int(bf_radix) {
			otherNum = num.convertToRadix(bf_radix)
		}
		
		// ignore invalid numbers
		if !otherNum.bf_is_valid || !thisNum.bf_is_valid {
			thisNum.bf_is_valid = false
			return thisNum
		}
		
		// Handle differences in sign by calling addition instead
		if otherNum.bf_is_negative != thisNum.bf_is_negative {
			thisNum.bf_is_negative = !bf_is_negative
			thisNum = thisNum.add(otherNum)
			thisNum.bf_is_negative = !thisNum.bf_is_negative
			return thisNum
		}
		
		Real.BF_NormaliseNumbers(&thisNum, otherNum: &otherNum)
		
		// Compare the two values
		var compare = NSComparisonResult.OrderedSame
		for i in (0..<Real.BF_num_values).reverse() {
			if thisNum.bf_array[i] > otherNum.bf_array[i] {
				compare = .OrderedDescending
				break
			} else if thisNum.bf_array[i] < otherNum.bf_array[i] {
				compare = .OrderedAscending
				break
			}
		}
		
		if compare == .OrderedDescending {
			// Perform the subtraction
			for i in 0..<Real.BF_num_values {
				// Borrow from the next column if we need to
				if otherNum.bf_array[i] > thisNum.bf_array[i] {
					// Since we know that this num is greater than otherNum, then we know
					// that this will never exceed the bounds of the array
					var peek = 1
					while thisNum.bf_array[i + peek] == 0 {
						thisNum.bf_array[i + peek] = thisNum.bf_value_limit - 1
						peek++
					}
					thisNum.bf_array[i+peek]--
					thisNum.bf_array[i] += thisNum.bf_value_limit
				}
				thisNum.bf_array[i] = thisNum.bf_array[i] - otherNum.bf_array[i]
			}
		} else if compare == .OrderedAscending {
			// Change the sign of this num
			thisNum.bf_is_negative = !thisNum.bf_is_negative
			
			// Perform the subtraction
			for i in 0..<Real.BF_num_values {
				// Borrow from the next column if we need to
				if thisNum.bf_array[i] > otherNum.bf_array[i] {
					// Since we know that this num is greater than otherNum, then we know
					// that this will never exceed the bounds of the array
					var peek = 1
					while otherNum.bf_array[i + peek] == 0 {
						otherNum.bf_array[i + peek] = otherNum.bf_value_limit - 1
						peek++
					}
					otherNum.bf_array[i+peek]--
					otherNum.bf_array[i] += otherNum.bf_value_limit
				}
				thisNum.bf_array[i] = otherNum.bf_array[i] - thisNum.bf_array[i];
			}
		} else {
			// Zero the exponent and remove the sign
			thisNum.bf_exponent = 0
			thisNum.bf_is_negative = false
			
			// Subtraction results in zero
			Real.BF_ClearValuesArray(&thisNum.bf_array)
		}
		
		// Create a user pont, store all the values back in the class and we're done
		thisNum.createUserPoint()
		return thisNum
	}
	
	//
	// multiplyBy
	//
	// I take the 8 seeds out of my apple. I plant them in the ground and grow 8 trees.
	// Each tree has 8 apples, how successful is my orchard?
	//
	public func multiplyBy(num: Real) -> Real {
		var thisNum = self
		var otherNum = num
		var result = [Digit](count: Real.BF_num_values * 2, repeatedValue: 0)
		
		if num.radix != Int(bf_radix) {
			otherNum = num.convertToRadix(bf_radix)
		}
		
		// ignore invalid numbers
		if !otherNum.bf_is_valid || !thisNum.bf_is_valid {
			thisNum.bf_is_valid = false
			return thisNum
		}
		
		// Apply the user's decimal point
		thisNum.bf_exponent -= Int(thisNum.bf_user_point)
		thisNum.bf_user_point = 0
		otherNum.bf_exponent -= Int(otherNum.bf_user_point)
		otherNum.bf_user_point = 0
		
		// Multiply exponents through addition
		thisNum.bf_exponent += otherNum.bf_exponent;
		
		// Two negatives make a positive
		if otherNum.bf_is_negative { thisNum.bf_is_negative = thisNum.bf_is_negative ? false : true }
		
		// Now we do the multiplication. Basic stuff:
		// Multiply each column of each of the otherNums by each other and sum all of the results
		var carryBits : Digit = 0
		for j in 0..<Real.BF_num_values {
			// Add the product of this column of otherNum with this num
			carryBits = 0
			for i in 0..<Real.BF_num_values	{
				result[i + j] += (thisNum.bf_array[i] * otherNum.bf_array[j]) + carryBits
				carryBits = result[i + j] / thisNum.bf_value_limit
				result[i + j] = result[i + j] % thisNum.bf_value_limit
			}
			
			// Add the carry for the last multiplication to the next column
			result[j + Real.BF_num_values] += carryBits
		}
		
		// If we have exceeded the precision, divide by the bf_radix until
		// we are reeled back in.
        while Real.BF_ArrayIsNonZero(result, start:Real.BF_num_values) {
			carryBits = Real.BF_RemoveDigitFromMantissa(&result, radix: thisNum.bf_radix, limit: thisNum.bf_value_limit, scale:2)
			thisNum.bf_exponent++
		}
		
		// Apply round to nearest
		if Double(carryBits) >= Double(bf_radix) / 2.0 {
			Real.BF_AddToMantissa(&result, digit: 1, limit: thisNum.bf_value_limit)
			
			// If by shear fluke that caused the top digit to overflow, then shift back by one digit
			if (result[Real.BF_num_values - 1] > thisNum.bf_value_limit) {
				Real.BF_RemoveDigitFromMantissa(&result, radix: thisNum.bf_radix, limit: thisNum.bf_value_limit)
				thisNum.bf_exponent++
			}
		}
		
		// Create a user pont, store all the values back in the class and we're done
		Real.BF_CopyValues(result, destination: &thisNum.bf_array)
		thisNum.createUserPoint()
		return thisNum
	}
    
    //
    // divideBy
    //
    // You see my orchard and get jealous. You start a fire that takes out half my crop.
    // How do you explain your actions to the judge?
    //
    public func divideBy(num: Real) -> Real {
		var values = [Digit](count: Real.BF_num_values * 2, repeatedValue: 0)
		var otherNumValues = values
		var result = values
		var subValues = values
		var thisNum = self
		var otherNum = num
		
		if num.radix != Int(bf_radix) {
			otherNum = num.convertToRadix(bf_radix)
		}
		
		// Get the numerical values
        Real.BF_CopyValues(bf_array, destination: &values, dstart: Real.BF_num_values)
        Real.BF_CopyValues(otherNum.bf_array, destination: &otherNumValues, dstart: Real.BF_num_values)
		
		// ignore invalid numbers
		if !otherNum.bf_is_valid || !thisNum.bf_is_valid {
			thisNum.bf_is_valid = false
			return thisNum
		}
		
		// Apply the user's decimal point
		thisNum.bf_exponent -= Int(thisNum.bf_user_point)
		thisNum.bf_user_point = 0
		otherNum.bf_exponent -= Int(otherNum.bf_user_point)
		otherNum.bf_user_point = 0
		
		// Two negatives make a positive
		if otherNum.bf_is_negative { thisNum.bf_is_negative = thisNum.bf_is_negative ? false : true }
		
		// Normalise this num
		// This involves multiplying through by the bf_radix until the number runs up against the
		// left edge or MSD (most significant digit)
        if Real.BF_ArrayIsNonZero(values) {
			while values[Real.BF_num_values * 2 - 1] < (thisNum.bf_value_limit / Digit(thisNum.bf_radix)) {
                Real.BF_AppendDigitToMantissa(&values, digit: 0, radix: thisNum.bf_radix, limit: thisNum.bf_value_limit, scale:2)
				thisNum.bf_exponent--
			}
		} else {
            Real.BF_AssignValues(&thisNum.bf_array, source: values, sstart:Real.BF_num_values)
			thisNum.bf_exponent = 0
			thisNum.bf_user_point = 0
			thisNum.bf_is_negative = false
			
			if !Real.BF_ArrayIsNonZero(otherNumValues) {
				thisNum.bf_is_valid = false
			}
			
			return thisNum
		}
		
		// We have the situation where otherNum had a larger kNumValue'th digit than
		// this num did in the first place. So we may have to divide through by bf_radix
		// once to normalise otherNum
		if (otherNumValues[Real.BF_num_values * 2 - 1] > values[Real.BF_num_values * 2 - 1]) {
            let carryBits = Real.BF_RemoveDigitFromMantissa(&otherNumValues, radix: thisNum.bf_radix, limit: thisNum.bf_value_limit, scale:2)
			otherNum.bf_exponent++
			
			if (Double(carryBits) >= (Double(otherNum.bf_radix) / 2.0)) {
				Real.BF_AddToMantissa(&otherNumValues, digit: 1, limit: otherNum.bf_value_limit, scale:2)
			}
		} else {
			// Normalise otherNum so that it cannot be greater than this num
			// This involves multiplying through by the bf_radix until the number runs up
			// against the left edge or MSD (most significant digit)
			// If the last multiply will make otherNum greater than this num, then we
			// don't do it. This ensures that the first division column will always be non-zero.
			if Real.BF_ArrayIsNonZero(otherNumValues) {
				while (otherNumValues[Real.BF_num_values * 2 - 1] < (otherNum.bf_value_limit / Digit(otherNum.bf_radix))) &&
					  (otherNumValues[Real.BF_num_values * 2 - 1] < (values[Real.BF_num_values * 2 - 1] / Digit(otherNum.bf_radix)))
				{
					Real.BF_AppendDigitToMantissa(&otherNumValues, digit: 0, radix: thisNum.bf_radix, limit: thisNum.bf_value_limit, scale:2)
					otherNum.bf_exponent--
				}
			} else {
				thisNum.bf_is_valid = false
				return thisNum
			}
		}
		
		// Subtract the exponents
		thisNum.bf_exponent -= otherNum.bf_exponent;
		
		// Account for the de-normalising effect of division
		thisNum.bf_exponent -= (Real.BF_num_values - 1) * Int(thisNum.bf_value_precision)
		
		// Begin the division
		// What we are doing here is lining the divisor up under the divisee and subtracting the largest multiple
		// of the divisor that we can from the divisee with resulting in a negative number. Basically it is what
		// you do without really thinking about it when doing long division by hand.
		var carryBits : Digit = 0
		for i in (Real.BF_num_values-1..<Real.BF_num_values * 2).reverse() {
			// If the divisor is greater or equal to the divisee, leave this result column unchanged.
			if otherNumValues[Real.BF_num_values * 2 - 1] > values[i] {
				if i > 0 {
					values[i - 1] += values[i] * thisNum.bf_value_limit
				}
				continue
			}
			
			// Determine the quotient of this position (the multiple of  the divisor to use)
			var quotient = values[i] / otherNumValues[Real.BF_num_values * 2 - 1];
			carryBits = 0
			for j in 0...i {
				subValues[j] = otherNumValues[j + (Real.BF_num_values * 2 - 1 - i)] * quotient + carryBits
				carryBits = subValues[j] / thisNum.bf_value_limit
				subValues[j] %= thisNum.bf_value_limit
			}
			subValues[i] += carryBits * thisNum.bf_value_limit
			
			// Check that values is greater than subValues (ie check that this subtraction won't
			// result in a negative number)
			var compare = NSComparisonResult.OrderedSame
			for j in (0...i).reverse() {
				if (values[j] > subValues[j]) {
					compare = .OrderedDescending
					break
				} else if (values[j] < subValues[j]) {
					compare = .OrderedAscending
					break
				}
			}
			
			// If we have overestimated the quotient, adjust appropriately. This just means that we need
			// to reduce the divisor's multiplier by one.
			while compare == .OrderedAscending {
				quotient--
				carryBits = 0
				for j in 0...i {
					subValues[j] = otherNumValues[j + (Real.BF_num_values * 2 - 1 - i)] * quotient + carryBits;
					carryBits = subValues[j] / thisNum.bf_value_limit;
					subValues[j] %= thisNum.bf_value_limit;
				}
				subValues[i] += carryBits * thisNum.bf_value_limit;
				
				// Check that values is greater than subValues (ie check that this subtraction won't
				// result in a negative number)
				compare = .OrderedSame
				for j in (0...i).reverse() {
					if (values[j] > subValues[j]) {
						compare = .OrderedDescending
						break
					} else if (values[j] < subValues[j]) {
						compare = .OrderedAscending
						break
					}
				}
			}
			
			// We now have the number to place in this column of the result. Yay.
			result[i] = quotient
			
			// If the subtraction operation will result in no remainder, then finish
			if compare == .OrderedSame {
				break
			}
			
			// Subtract the sub values from values now
			for j in (0..<Real.BF_num_values * 2).reverse() {
				if subValues[j] > values[j] {
					// Since we know that this num is greater than the sub num, then we know
					// that this will never exceed the bounds of the array
					var peek = 1
					while values[j + peek] == 0 {
						values[j + peek] = thisNum.bf_value_limit - 1
						peek++
					}
					values[j+peek]--
					values[j] += thisNum.bf_value_limit
				}
				values[j] -= subValues[j]
			}
			
			// Attach the remainder to the next column on the right so that it will be part of the next
			// column's operation
			values[i - 1] += values[i] * thisNum.bf_value_limit
			
			// Clear the remainder from this column
			values[i] = 0
			subValues[i] = 0
		}
		
		// Normalise the result
		// This involves multiplying through by the bf_radix until the number runs up against the
		// left edge or MSD (most significant digit)
		while result[Real.BF_num_values * 2 - 1] < (thisNum.bf_value_limit / Digit(thisNum.bf_radix)) {
			Real.BF_AppendDigitToMantissa(&result, digit: 0, radix: thisNum.bf_radix, limit: thisNum.bf_value_limit, scale:2)
			thisNum.bf_exponent--
		}
		
		// Apply a round to nearest on the last digit
		if ((Double(result[Real.BF_num_values - 1]) / Double(bf_value_limit / Digit(bf_radix))) >= (Double(bf_radix) / 2.0)) {
            Real.BF_AddToMantissa(&result, digit: 1, limit: thisNum.bf_value_limit, start: Real.BF_num_values)
			
			// If by shear fluke that cause the top digit to overflow, then shift back by one digit
			if (result[Real.BF_num_values - 1] > thisNum.bf_value_limit) {
				carryBits = Real.BF_RemoveDigitFromMantissa(&result, radix: thisNum.bf_radix, limit: thisNum.bf_value_limit, start: Real.BF_num_values)
				thisNum.bf_exponent++
				if Double(carryBits) >= (Double(thisNum.bf_radix) / 2.0) {
					Real.BF_AddToMantissa(&result, digit: 1, limit: thisNum.bf_value_limit, start: Real.BF_num_values)
				}
			}
		}
		
		// Remove any trailing zeros in the decimal places by dividing by the bf_radix until they go away
        carryBits = 0
		while (thisNum.bf_exponent < 0) && (result[Real.BF_num_values] % Digit(thisNum.bf_radix) == 0) {
			carryBits = Real.BF_RemoveDigitFromMantissa(&result, radix: thisNum.bf_radix, limit: thisNum.bf_value_limit, start: Real.BF_num_values)
			thisNum.bf_exponent++
		}
		if (Double(carryBits) >= (Double(thisNum.bf_radix) / 2.0)) {
			Real.BF_AddToMantissa(&result, digit: 1, limit: thisNum.bf_value_limit, start: Real.BF_num_values)
		}
		
		// Create a user point, store all the values back in the class and we're done
        Real.BF_AssignValues(&thisNum.bf_array, source: result, sstart: Real.BF_num_values)
		thisNum.createUserPoint()
		return thisNum
	}
	
    //
    // moduloBy
    //
    // The judge orders that that the orchard be divided between you, me and the judge. The
    // remaining tree is given to charity.
    //
    public func moduloBy(num: Real) -> Real {
        var values = [Digit](count: Real.BF_num_values * 2, repeatedValue: 0)
        var otherNumValues = values
        var result = values
        var subValues = values
        var thisNum = self
        var otherNum = num
        
        if num.radix != Int(bf_radix) {
            otherNum = num.convertToRadix(bf_radix)
        }
        
        // Get the numerical values
        Real.BF_CopyValues(bf_array, destination: &values, dstart: Real.BF_num_values)
        Real.BF_CopyValues(otherNum.bf_array, destination: &otherNumValues, dstart: Real.BF_num_values)
        
        // ignore invalid numbers
        if !otherNum.bf_is_valid || !thisNum.bf_is_valid {
            thisNum.bf_is_valid = false
            return thisNum
        }
        
        
        let compare = self.compareWith(num)
        if compare == .OrderedAscending {
            // return unchanged if num is less than the modulor
            return self
        }
        
        // Apply the user's decimal point
        thisNum.bf_exponent -= Int(thisNum.bf_user_point)
        thisNum.bf_user_point = 0
        otherNum.bf_exponent -= Int(otherNum.bf_user_point)
        otherNum.bf_user_point = 0
 
        // Two negatives make a positive
        if otherNum.bf_is_negative { thisNum.bf_is_negative = thisNum.bf_is_negative ? false : true }
        
        // Normalise this num
        // This involves multiplying through by the bf_radix until the number runs up against the
        // left edge or MSD (most significant digit)
        if Real.BF_ArrayIsNonZero(values) {
            while(values[Real.BF_num_values * 2 - 1] < (thisNum.bf_value_limit / Digit(thisNum.bf_radix))) {
                Real.BF_AppendDigitToMantissa(&values, digit: 0, radix: thisNum.bf_radix, limit: thisNum.bf_value_limit, scale:2)
                thisNum.bf_exponent--
            }
        } else {
            Real.BF_AssignValues(&thisNum.bf_array, source: values, sstart: Real.BF_num_values)
            thisNum.bf_exponent = 0
            thisNum.bf_user_point = 0
            thisNum.bf_is_negative = false
            return thisNum
        }
        
        // Normalise otherNum so that it cannot be greater than this num
        // This involves multiplying through by the bf_radix until the number runs up
        // against the left edge or MSD (most significant digit)
        // If the last multiply will make otherNum greater than this num, then we
        // don't do it. This ensures that the first division column will always be non-zero.
        if Real.BF_ArrayIsNonZero(otherNumValues) {
            while (otherNumValues[Real.BF_num_values * 2 - 1] < (otherNum.bf_value_limit / Digit(otherNum.bf_radix))) &&
                (otherNumValues[Real.BF_num_values * 2 - 1] < (values[Real.BF_num_values * 2 - 1] / Digit(otherNum.bf_radix)))
            {
                Real.BF_AppendDigitToMantissa(&otherNumValues, digit: 0, radix: thisNum.bf_radix, limit: thisNum.bf_value_limit, scale:2)
                otherNum.bf_exponent--
            }
        } else {
            thisNum.bf_is_valid = false
            return thisNum
        }
        
        // Subtract the exponents
        var divisionExponent = thisNum.bf_exponent - otherNum.bf_exponent
        
        // Account for the de-normalising effect of division
        divisionExponent -= (Real.BF_num_values - 1) * Int(thisNum.bf_value_precision)
        
        // Set the re-normalised values so that we can subtract from self later
        var nself = thisNum
        Real.BF_AssignValues(&nself.bf_array, source:values, sstart:Real.BF_num_values)
     
        // Begin the division
        // What we are doing here is lining the divisor up under the divisee and subtracting the largest multiple
        // of the divisor that we can from the divisee with resulting in a negative number. Basically it is what
        // you do without really thinking about it when doing long division by hand.
        var carryBits : Digit = 0
        for i in (Real.BF_num_values-1..<Real.BF_num_values * 2).reverse()  {
            // If the divisor is greater or equal to the divisee, leave this result column unchanged.
            if otherNumValues[Real.BF_num_values * 2 - 1] > values[i] {
                if i > 0 {
                    values[i - 1] += values[i] * thisNum.bf_value_limit
                }
                continue
            }
            
            // Determine the quotient of this position (the multiple of  the divisor to use)
            var quotient = values[i] / otherNumValues[Real.BF_num_values * 2 - 1];
            carryBits = 0
            for j in 0...i {
                subValues[j] = otherNumValues[j + (Real.BF_num_values * 2 - 1 - i)] * quotient + carryBits
                carryBits = subValues[j] / thisNum.bf_value_limit
                subValues[j] %= thisNum.bf_value_limit
            }
            subValues[i] += carryBits * thisNum.bf_value_limit;
            
            // Check that values is greater than subValues (ie check that this subtraction won't
            // result in a negative number)
            var compare = NSComparisonResult.OrderedSame
            for j in (0...i).reverse() {
                if (values[j] > subValues[j]) {
                    compare = .OrderedDescending
                    break
                } else if (values[j] < subValues[j]) {
                    compare = .OrderedAscending
                    break
                }
            }
            
            // If we have overestimated the quotient, adjust appropriately. This just means that we need
            // to reduce the divisor's multiplier by one.
            while compare == .OrderedAscending {
                quotient--
                carryBits = 0
                for j in 0...i {
                    subValues[j] = otherNumValues[j + (Real.BF_num_values * 2 - 1 - i)] * quotient + carryBits;
                    carryBits = subValues[j] / thisNum.bf_value_limit;
                    subValues[j] %= thisNum.bf_value_limit;
                }
                subValues[i] += carryBits * thisNum.bf_value_limit;
                
                // Check that values is greater than subValues (ie check that this subtraction won't
                // result in a negative number)
                compare = .OrderedSame
                for j in (0...i).reverse() {
                    if (values[j] > subValues[j]) {
                        compare = .OrderedDescending
                        break
                    } else if (values[j] < subValues[j]) {
                        compare = .OrderedAscending
                        break
                    }
                }
            }
            
            // We now have the number to place in this column of the result. Yay.
            result[i] = quotient
            
            // If the subtraction operation will result in no remainder, then finish
            if (compare == .OrderedSame) {
                break
            }
            
            // Subtract the sub values from values now
            for j in 0..<Real.BF_num_values * 2 {
                if (subValues[j] > values[j]) {
                    // Since we know that this num is greater than the sub num, then we know
                    // that this will never exceed the bounds of the array
                    var peek = 1
                    while (values[j + peek] == 0) {
                        values[j + peek] = thisNum.bf_value_limit - 1
                        peek++
                    }
                    values[j+peek]--
                    values[j] += thisNum.bf_value_limit
                }
                values[j] -= subValues[j]
            }
            
            // Attach the remainder to the next column on the right so that it will be part of the next
            // column's operation
            values[i - 1] += values[i] * thisNum.bf_value_limit
            
            // Clear the remainder from this column
            values[i] = 0
            subValues[i] = 0
        }
        
        // Remove the fractional part of the division result
        // We know that there must be a non-fractional part since the modulor was tested to
        // be less or equal to the modulee
        while divisionExponent < 0 {
            carryBits = Real.BF_RemoveDigitFromMantissa(&result, radix: thisNum.bf_radix, limit: thisNum.bf_value_limit, start: Real.BF_num_values)
            result[Real.BF_num_values - 1] += carryBits * thisNum.bf_value_limit;
            carryBits = Real.BF_RemoveDigitFromMantissa(&result, radix: thisNum.bf_radix, limit: thisNum.bf_value_limit)
            divisionExponent++
        }
        
        // Now create a number that is this dividend times the modulor and subtract it from the
        // modulee to obtain the result
        var subNum = zero
        subNum.setElements(thisNum.bf_radix, negative:thisNum.bf_is_negative, exp:divisionExponent, valid:true, userPoint:0)
        subNum.bf_array = Array(result[Real.BF_num_values..<result.count])
        subNum = subNum.multiplyBy(num)
        nself = nself.subtract(subNum)
        
        // Remove any trailing zeros in the decimal places by dividing by the bf_radix until they go away
        thisNum = nself
        Real.BF_CopyValues(nself.bf_array, destination: &values)
        while (thisNum.bf_exponent < 0) && (values[0] % Digit(thisNum.bf_radix) == 0) {
            carryBits = Real.BF_RemoveDigitFromMantissa(&values, radix: thisNum.bf_radix, limit: thisNum.bf_value_limit)
            nself.bf_exponent++
        }
        
        // Create a user point, store all the values back in the class and we're done
        nself = thisNum
        Real.BF_AssignValues(&nself.bf_array, source: values)
        nself.createUserPoint()
        return nself
    }

	
	//
	// Returns the scale of the receiver with respect to num.
	//
	public func compareWith(num: Real) -> NSComparisonResult {
        var values : Real = self
        var otherNum : Real = num
        var compare: NSComparisonResult
        
        if num.radix != Int(bf_radix) {
            otherNum.convertToRadix(bf_radix)
        }
        
        // ignore invalid numbers
        if (otherNum.bf_is_valid == false || bf_is_valid == false) {
            return .OrderedAscending
        }
        
        // Handle differences in sign
        if otherNum.bf_is_negative != bf_is_negative {
            if otherNum.bf_is_negative { return .OrderedDescending }
            else { return .OrderedAscending }
        }
        
        Real.BF_NormaliseNumbers(&values, otherNum: &otherNum)
        
        let ownLength = Real.BF_NumDigitsInArray(values.bf_array, radix: bf_radix, precision: Digit(bf_value_precision))
        let otherLength = Real.BF_NumDigitsInArray(otherNum.bf_array, radix: bf_radix, precision: Digit(bf_value_precision))
        let maxLength = (ownLength > otherLength) ? ownLength : otherLength
        
        //
        // For a full length number, never compare the last digit because it's
        // subject to rounding problems.
        //
        if maxLength == Int(bf_value_precision) * Real.BF_num_values {
            values.bf_array[0] /= Digit(bf_radix)
            values.bf_array[0] *= Digit(bf_radix)
            otherNum.bf_array[0] /= Digit(bf_radix)
            otherNum.bf_array[0] *= Digit(bf_radix)
        }
        
        // Now that we're normalised, do the actual comparison
        compare = .OrderedSame
        for i in (0..<Real.BF_num_values).reverse() {
            if (values.bf_array[i] > otherNum.bf_array[i] && !bf_is_negative) || (values.bf_array[i] < otherNum.bf_array[i] && bf_is_negative) {
                compare = .OrderedDescending
                break
            } else if (values.bf_array[i] < otherNum.bf_array[i] && !bf_is_negative) || (values.bf_array[i] > otherNum.bf_array[i] && bf_is_negative) {
                compare = .OrderedAscending
                break
            }
        }
        
        return compare
	}
    
    // MARK: - Extended Mathematics Functions
    
    //
    // Raises the receiver to the exponent "num".
    //
    public func raiseToPower(num: Real) -> Real {
        var numCopy = num
        var result = self
        var negative = false
        
        if !bf_is_valid { return self }
        
        if !num.isValid {
            result.bf_is_valid = false
            return result
        }
        
        if self.isZero {
            // Zero raised to anything except zero is zero (provided exponent is valid)
            numCopy.bf_is_valid = num.isValid
            if num.isZero { return one }
            return self
        }
        
        let exp = Int(num.doubleValue)
        if num.fractionalPart.isZero && Swift.abs(exp) < 0x8000 {
            return self.raiseToIntPower(exp)
        }
        
        if bf_is_negative {
            result.bf_is_negative = false
            negative = true
        }
        
        result = (result.ln() * num).powerOfE()
        if negative {
            if numCopy.isNegative {
                numCopy = -numCopy
            }
            numCopy %= two
            if numCopy == one {
                result.bf_is_negative = true
            } else if !numCopy.isZero {
                result.bf_is_valid = false
            }
            
        }
        return result
    }
    
    //
    // Returns the value e^x where x is the value of the receiver.
    //
    public func powerOfE() -> Real {
        var squares = 0
        var result = self
        
        if !bf_is_valid { return result }
        
        // Pre-scale the number to aid convergeance
        if bf_is_negative {
            result.bf_is_negative = false
        }
		
		while result > one {
            result /= two
            squares++
        }
        
        // Initialise stuff
        var factorialValue = one
        var prevIteration = one
        let original = result
        var powerCopy = result
        
        // Set the current value to 1 (the zeroth term)
        result = one
        
        // Add the second term
        result += original
        
        // otherwise iterate the Taylor Series until we obtain a stable solution
        var i = two
        var nextTerm = factorialValue
        while result != prevIteration {
            // Get a copy of the current value so that we can see if it changes
            prevIteration = result
            
            // Determine the next term of the series
            powerCopy *= original
            
            factorialValue *= i
            nextTerm = powerCopy / factorialValue
            
            // Add the next term if it is valid
            if nextTerm.isValid {
                result += nextTerm
            }
            
            i += one
        }
        
        // Reverse the prescaling
        while squares > 0 {
            result *= result
            squares--
        }
        
        if bf_is_negative {
            return result.inverse
        }
        return result
    }
    
    public func sqrt() -> Real { return nRoot(2) }
    public func cbrt() -> Real { return nRoot(3) }
    
    //
    // Takes the nth root of the receiver
    //
    public func nRoot(n: Int) -> Real {
        var result = self
        
        if !bf_is_valid || self.isZero { return self }
        
        // oddly-numbered roots of negative numbers should work
        if self.isNegative && (n & 1) == 0 {
            result.bf_is_valid = false
            return result
        }
        result.bf_is_negative = false     // we'll fix this later
        
        let original = result
        let root = Real(n, radix: bf_radix)
        
        // Count the number of digits left of the point
        var numDigits = Real.BF_num_values * Int(bf_value_precision) + Int(bf_exponent) - Int(bf_user_point)
        var digitNotFound = true
        for i in (0..<Real.BF_num_values).reverse() where digitNotFound {
            for j in (0..<bf_value_precision).reverse() where digitNotFound {
                if ((bf_array[i] / Real.pow(bf_radix, Int(j)) % Digit(bf_radix)) == 0) {
                    numDigits--
                } else {
                    digitNotFound = false
                }
            }
        }
        
        // The first guess will be the scaled exponent of this number
        result.bf_exponent -= numDigits / n
        var prevGuess = result + one
        var newGuess = result
        
        // Do some Newton's method iterations until we converge
        var maxIterations = 1000
        var power = zero
        while newGuess != prevGuess && maxIterations > 0 {
            prevGuess = newGuess
            
            newGuess = original
            power = prevGuess.raiseToIntPower(n-1)
            newGuess /= power
            newGuess -= prevGuess
            newGuess /= root
            newGuess += prevGuess
            maxIterations--
        }
        if maxIterations <= 0 { NSLog("Exceeded iteration limit on root evaluation: Error is likely") }
        
        // Use the last guess
        result = newGuess
        
        //
        // This method has problems actually giving "1" as a result. If we're
        // within n * smallest digit size, then round to one.
        //
        let twoEpsilon = Real(mantissa: 2, exponent: -(Real.BF_num_values * Int(bf_value_precision)) + 1, isNegative: false, radix: bf_radix, userPointAt: 0)
		if (result - one).abs() < twoEpsilon {
			result = one
		}
		
        // fix the sign
        result.bf_is_negative = bf_is_negative
        return result
    }

    
    //
    // Returns the natural logarithm of the receiver.
    //
    public func ln() -> Real {
        if !bf_is_valid { return self }
        
        var prevIteration = zero
        var i = two
        let eighth = Real(0.125, radix: bf_radix)
        var result = self
        var inverse = false
        var outputFactor : UInt64 = 1
        
        // ln(x) for x <= 0 is inValid
        if self <= prevIteration {
            result.bf_is_valid = false
            return result
        }
        
        // ln(x) for x > 1 == -ln(1/x)
        if self > one {
            result = self.inverse
            inverse = true
        }
        
        // Shift the number into a range between 1/8th and 1 (helps convergeance to a solution)
        while result < eighth {
            result = result.sqrt()
            outputFactor *= 2
        }
        
        // The base of our power is (x-1)
        // This value is also the first term
        result -= one
        let original = result
        var powerCopy = result
        var nextTerm = result
        
        // iterate the Taylor Series until we obtain a stable solution
        while result != prevIteration {
            // Get a copy of the current value so that we can see if it changes
            prevIteration = result
            
            // Determine the next term of the series
            powerCopy *= original
            nextTerm = powerCopy
            nextTerm /= i
            
            // Subtract the next term if it is valid
            if nextTerm.isValid {
                result -= nextTerm
            }
            
            i += one
            
            // Determine the next term of the series
            powerCopy *= original
            nextTerm = powerCopy
            nextTerm /= i
            
            // Add the next term if it is valid
            if nextTerm.isValid {
                result += nextTerm
            }
            
            i += one
        }
        
        if inverse {
            result.appendDigit("-", useComplement:0)
        }
        
        // Descale the result
        let factorNum = Real(Int(outputFactor), radix: bf_radix)
        return result * factorNum
    }
    
    //
    // Takes the "base" log of the receiver.
    //
    public func logOfBase(base: Real) -> Real {
        if !bf_is_valid { return self }
        if !base.isValid { return base }
        return self.ln()/base.ln()
    }
    
    //
    // Returns the sine of the receiver where the receiver
    // is interpreted as having *mode* angular units.
    //
    public func sinWithTrigMode(mode: BFTrigMode) -> Real {
        if !bf_is_valid { return self }
		
        var result = self.toRadiansFrom(mode)
        
        var prevIteration = zero
        var factorial = one
        let original = result
        var powerCopy = result
        var nextTerm = result
        
        var i = 1
        while result != prevIteration {
            // Get a copy of the current value so that we can see if it changes
            prevIteration = result
            
            // Determine the next term of the series
            // Numerator is x^(2n+1)
            powerCopy *= original * original
            nextTerm = powerCopy
            
            // Divide the term by (2n+1)!
            let twoN = Real(i * 2, radix: bf_radix)
            let twoNPlusOne = Real(i * 2 + 1, radix: bf_radix)
            factorial *= twoN
            factorial *= twoNPlusOne
            nextTerm /= factorial
            
            // Add/subtract the next term if it is valid
            if nextTerm.isValid {
                if i % 2 == 0 { result += nextTerm }
                else          { result -= nextTerm }
            }
            
            i++
        }
        
        // Check that accurracy hasn't caused something illegal
        let value = result.abs()
        if value > one { result /= value }
        
        // Normalise to remove built up error (makes a zero output possible)
        nextTerm = Real(10000, radix:bf_radix)
        Real.BF_NormaliseNumbers(&result, otherNum: &nextTerm)
        result.createUserPoint()
        return result
    }
    
    //
    // Returns the cosine of the receiver where the receiver
    // is interpreted as having *mode* angular units.
    //
    func cosWithTrigMode(mode: BFTrigMode) -> Real {
        if !bf_is_valid { return self }
        
        var result = self.toRadiansFrom(mode)
        
        var prevIteration = zero
        var factorial = one
        let original = result
        var powerCopy = factorial
        var nextTerm = factorial
        
        result = factorial
        
        var i = 1
        while result != prevIteration {
            // Get a copy of the current value so that we can see if it changes
            prevIteration = result
            
            // Determine the next term of the series
            // Numerator is x^(2n)
            powerCopy *= original
            powerCopy *= original
            nextTerm = powerCopy
            
            // Divide the term by (2n)!
            let twoN = Real(i * 2, radix:bf_radix)
            let twoNMinusOne = Real(i * 2 - 1, radix:bf_radix)
            factorial *= twoN
            factorial *= twoNMinusOne
            nextTerm /= factorial
            
            // Add/subtract the next term if it is valid
            if nextTerm.isValid {
                if i % 2 == 0 { result += nextTerm }
                else          { result -= nextTerm }
            }
            
            i++
        }
        
        // Check that accurracy hasn't caused something illegal
        let value = result.abs()
        if value > one { result /= value }
        
        // Normalise to remove built up error (makes a zero output possible)
        nextTerm = Real(10000, radix:bf_radix)
        Real.BF_NormaliseNumbers(&result, otherNum: &nextTerm)
        result.createUserPoint()
        return result
    }
    
    //
    // Returns the tangent of the receiver where the receiver
    // is interpreted as having *mode* angular units.
    //
    public func tanWithTrigMode(mode: BFTrigMode) -> Real {
        if !bf_is_valid { return self }
        
        let original = self.toRadiansFrom(mode)
        var result = original.sinWithTrigMode(mode) / original.cosWithTrigMode(mode)
        
        // Normalise to remove built up error (makes a zero output possible)
        var nextTerm = Real(10000, radix:bf_radix)
        Real.BF_NormaliseNumbers(&result, otherNum: &nextTerm)
        result.createUserPoint()
        return result
    }

    public func asin(mode: BFTrigMode) -> Real {
        if !bf_is_valid { return self }
        
        let half = Real(0.5, radix:bf_radix)
        let minusHalf = Real(-0.5, radix:bf_radix)
        
        // To speed convergeance, for x >= 0.5, let asin(x) = pi/2-2*asin(sqrt((1-x)/2))
        var arcsinShift = false
        var signChange = false
        var result = self
        if result > half {
            result.appendDigit("-", useComplement:0)
            result += one
            result /= two
            result = result.sqrt()
            arcsinShift = true
        }
        if result < minusHalf {
            signChange = true
            result += one
            result /= two
            result = result.sqrt()
            arcsinShift = true
        }
        
        var prevIteration = zero
        var factorial = one
        var original = result
        var powerCopy = result
        var nextTerm = result
        
        var i = 1
        while result != prevIteration {
            // Get a copy of the current value so that we can see if it changes
            prevIteration = result
            
            // Determine the next term of the series
            powerCopy *= original
            powerCopy *= original
            let twoN = Real(i * 2, radix:bf_radix)
            let twoNMinusOne = Real(i * 2 - 1, radix:bf_radix)
            factorial *= twoNMinusOne
            factorial /= twoN
            
            nextTerm = powerCopy
            let twoNPlusOne = Real(i * 2 + 1, radix:bf_radix)
            nextTerm /= twoNPlusOne
            nextTerm *= factorial
            
            if nextTerm.isValid { result += nextTerm }
            i++
        }
        
        if arcsinShift {
            result *= four
            result.appendDigit("-", useComplement:0)
            result += π
            result /= two
        }
        
        if signChange { result.appendDigit("-", useComplement:0) }
        
        // Check that accurracy hasn't caused something illegal
        original = π / two
        if result > original { result = original }
        original.appendDigit("-", useComplement:0)
        if result < original { result = original }
        return result.radiansToMode(mode)
    }
    
    public func acos(mode: BFTrigMode) -> Real {
        // arccos = π/2 - arcsin
        if !bf_is_valid { return self }
        var original = self.asin(.BF_radians)
        original = π / two - original
        return original.radiansToMode(mode)
    }
    
    public func atan(mode: BFTrigMode) -> Real {
        if !bf_is_valid { return self }
        let minusOne = Real(-1, radix: bf_radix)
        var original = self
        var powerCopy = original
        var factorial = one
        var nextTerm = factorial
        var result = self
        
        var path = 1
        let compare = original.compareWith(one)
        if compare == .OrderedDescending {
            path = 2
        } else if compare != .OrderedSame {
            if original < minusOne {
                path = 3
            }
        } else {
            // atan(1) = pi/4
            original = π / two / two
            path = 4
        }
        
        if path == 1  { // inverse tangent for |x| < 1
            var prevIteration = one
            
            while result != prevIteration {
                prevIteration = result
                
                factorial += two
                powerCopy *= original * original
                nextTerm = powerCopy
                nextTerm /= factorial
                if nextTerm.isValid { result -= nextTerm }
                
                factorial += two
                powerCopy *= original * original
                nextTerm = powerCopy
                nextTerm /= factorial
                if nextTerm.isValid { result += nextTerm }
            }
        } else if path != 4 { // inverse tangent for |x| >= 1
            // arctan = ((x>=1) * -1)π/2 - 1/x + 1/(3x^3) - 1/(5x^5) +...
            
            // generate the (+/-) π/2
            result = π / two
            if path == 3 {
                result.appendDigit("-", useComplement:0)
            }
            var prevIteration = result
            
            // Apply the first term
            nextTerm = original.inverse
            result -= nextTerm
            
            while result != prevIteration {
                prevIteration = result
                
                factorial += two
                powerCopy *= original * original
                nextTerm = factorial
                nextTerm *= powerCopy
                nextTerm = nextTerm.inverse
                if nextTerm.isValid { result += nextTerm }
                
                factorial += two
                powerCopy *= original * original
                nextTerm = factorial
                nextTerm *= powerCopy
                nextTerm = nextTerm.inverse
                if nextTerm.isValid { result += nextTerm }
            }
        }
        
        return result.radiansToMode(mode)
    }

    public func sinh() -> Real {
        if !bf_is_valid { return self }
        var result = self
        var original = self.powerOfE()
        result.appendDigit("-", useComplement:0)
        result = result.powerOfE()
        original -= result
        original /= two
        return original
    }

    public func cosh() -> Real {
        if !bf_is_valid { return self }
        var original = self.powerOfE()
		var result = self
        result.bf_is_negative = !bf_is_negative ? true : false
        original += result.powerOfE()
        original /= two
        return original
    }

    public func tanh() -> Real {
        if !bf_is_valid { return self }
        return self.sinh() / self.cosh()
    }
    
    public func asinh() -> Real {
        if !bf_is_valid { return self }
        var original = self
        let result = (self * self + one).sqrt()
        original += result
        return original.ln()
    }
    
    public func acosh() -> Real {
        if !bf_is_valid { return self }
        var original = self
		let result = (self * self - one).sqrt()
        original += result
		return original.ln()
    }
    
    public func atanh() -> Real {
        if !bf_is_valid { return self }
        var result = (one + self) / (one - self)
        result = result.ln()
        return result / two
    }
    
    public func atan2(y:Real) -> Real {
        let x = self
        let k3 = Real(3, radix: bf_radix)
        let k4 = four
        
        // Algorithm shamelessly stolen from qd_real
        if x.isZero {
            if y.isZero {
                // Actually an error
                var x = zero
                x.bf_is_valid = false
                return x
            }
            return y.isNegative ? -π : π
        } else if y.isZero {
            return x.isNegative ? π : zero
        }
        
        if x == y {
            return y.isNegative ? -k3 * π / k4 : π / k4
        }
        
        if x == -y {
            return y.isNegative ? -π / k4 : k3 * π / k4
        }
        return (y/x).atan(.BF_radians)
    }
    
    func hypot(y:Real) -> Real {
        var x = self
        x = x*x + y*y
        return x.sqrt()
    }
    
    //
    // Calculates a factorial in the most basic way.
    //
    public func factorial() -> Real {
        if !bf_is_valid { return self }
        
        var result = self
        
        // Negative numbers have no factorial equivalent
        if bf_is_negative || !fractionalPart.isZero {
            result.bf_is_valid = false
            return result
        }
        
        // Factorial zero is 1
        if isZero {
            return one
        } else {
            // Copy this num and start subtracting down to one
            var counter = self - one
            
            // Perform the basic factorial
            while counter > zero && result.bf_is_valid {
                result *= counter
                counter -= one
            }
            return result
        }
    }
    
    //
    // Permutation of receiver samples made from a range of r options
    //
    public func nPr(r: Real) -> Real {
        if !bf_is_valid { return self }
        if !r.isValid { return r }
        
        var result = self.wholePart
        if result < r {
            return zero
        }
        
        let self_minus_r = result - r.wholePart
        
        result = result.factorial()
        result /= self_minus_r.factorial()
        return result
        
    }
    
    //
    // Calculates receiver combinations of samples taken from a choice of r candidates.
    //
    public func nCr (r: Real) -> Real {
        if !bf_is_valid { return self }
        if !r.isValid { return r }
        
        var result = self.wholePart
        if result < r {
            return zero
        }
        
        let rcopy = r.wholePart
        let self_minus_r = result - rcopy
        
        result = result.factorial()
        result /= self_minus_r.factorial()
        result /= rcopy.factorial()
        return result
    }

    
    //
    // Performs 1/receiver.
    //
    public var inverse: Real {
		if !bf_is_valid { return self }
        if isZero {
            var result = self
            result.bf_is_valid = false
            return result
        }
        return one.divideBy(self)
    }
	
	public func andWith(num: Real, usingComplement complement: Int) -> Real {
		return self.opWith(num, usingComplement: complement, andOp: &)
	}
	
	public func orWith(num: Real, usingComplement complement: Int) -> Real {
		return self.opWith(num, usingComplement: complement, andOp: |)
	}
	
	public func xorWith(num: Real, usingComplement complement: Int) -> Real {
		return self.opWith(num, usingComplement: complement, andOp: ^)
	}
	
	private func not(a: Digit, b: Digit) -> Digit {
		// b is a dummy just so the binary op algorithm can be applied here as well
		return ~a
	}
	
	public func notUsingComplement(complement: Int) -> Real {
		return self.opWith(zero, usingComplement: complement, andOp: not)
	}
    
    // MARK: - Accessor Functions
    
    //
    // Sets the receiver to the receiver modulo 1.
    //
    public var fractionalPart: Real {
        if !bf_is_valid { return self}
        return self.moduloBy(one)
    }
    
    //
    // Sets the receiver to (receiver - (receiver modulo 1))
    //
    public var wholePart: Real {
        if !bf_is_valid { return self }
        
        let isNegative = bf_is_negative
        let numb = self.abs()
        
        var whole = numb.subtract(numb.fractionalPart)
        whole.bf_is_negative = isNegative
        return whole
    }
    
    //
    // Returns the approximate value of the receiver as a double
    //
    public var doubleValue: Double {
        // Return NaN if number is not valid
        if !bf_is_valid {
            return Double.NaN
        }
        
        // Extract all the digits out and put them in the double
        var retVal = 0.0
        for i in (0..<Real.BF_num_values).reverse() {
            let currentValue = Int64(bf_array[i])
            for j in (0..<bf_value_precision).reverse() {
                let power : Digit = Real.pow(bf_radix, Int(j))
                let digit = currentValue / Int64(power) % Int64(bf_radix)
                retVal = (retVal * Double(bf_radix)) + Double(digit)
            }
        }
        
        // Apply the sign
        if bf_is_negative {
            retVal *= -1.0
        }
        
        // Apply the exponent
        retVal *= Real.pow(bf_radix, bf_exponent - Int(bf_user_point))
        return retVal
    }
	
	//
	// Interprets the given int as an exponent and formats it as a string. Why did I do this?
	//
	public func exponentStringFromInt(exp: Int) -> String {
        var workingExponent = exp;
        var exponentIsNegative = false
        var digits = [Character](count: Real.BF_max_exponent_length, repeatedValue: "0")
        var currentPosition = Real.BF_max_exponent_length - 1  // index of the end of the string
        var lastNonZero = currentPosition
        
        if workingExponent == 0 { return "" }
        
        // Check for a negative exponent
        if workingExponent < 0 {
            workingExponent = -workingExponent
            exponentIsNegative = true
        }
        
        // Work right to left and fill in the digits
        while currentPosition > Real.BF_max_exponent_length - Int(bf_value_precision) - 2 {
            digits[currentPosition] = Real.BF_digits[workingExponent % bf_radix]
            
            // Keep checking for the leftmost non-zero digit
            if digits[currentPosition] != "0" {
                lastNonZero = currentPosition
            }
            
            workingExponent /= bf_radix
            currentPosition--
        }
        
        // If all the digits were zeros, force the display of at least one zero
        if lastNonZero == Real.BF_max_exponent_length {
            lastNonZero--
        }
        
        // Don't display any superfluous leading zeros
        let returnString = String(digits[lastNonZero..<digits.count])
        
        // Apply the sign
        if exponentIsNegative {
            return "-" + returnString
        }
        
        // Return the string
        return returnString
	}
    
    //
    // Returns the exponent of the receiver formatted as a string
    //
    public var exponentString : String {
        // Check to see if the exponent exists
        if !bf_is_valid {
            // Return an empty string instead of nil because its a little safer
            return ""
        }
        return exponentStringFromInt(bf_exponent)
    }
    
    //
    // Returns the mantissa of the receiver as a string.
    //
    public var mantissaString: String {
        var mantissa = ""
        var exponent = ""
        
        limitedString(Real.BF_num_values * Int(bf_value_precision), fixedPlaces:0, fillLimit:false, complement:0, mantissa:&mantissa, exponent:&exponent)
        
        return mantissa
    }
    
    //
    // Returns an approximate string representation of the receiver
    //
    public var description: String {
        // Get the mantissa string
        let mantissa = mantissaString
        
        // Append the exponent string
        if bf_exponent != 0 {
            return mantissa + "e" + exponentString
        }
        
        return mantissa
    }
    
    //
    // Returns a very short approximate value of the receiver as a string
    //
    public func toShortString(precision: Int) -> String {
        var string = ""
        var exponent = ""
        
        // Get the string pieces
        limitedString(4, fixedPlaces:0, fillLimit:false, complement:0, mantissa:&string, exponent:&exponent)
        
        // Append the exponent string
        if exponent.count() != 0 {
            string += "e" + exponent
        }
        
        return string
    }
	
    //
    // Returns the mantissa and exponent of the receiver as strings with specific formatting
    // according to the information provided.
    //
    private func limitedString(var lengthLimit: Int, var fixedPlaces places:Int, fillLimit fill:Bool, complement:UInt, inout mantissa mantissaOut:String, inout exponent exponentOut:String) {
        var digits = [Character](count: Real.BF_max_mantissa_length, repeatedValue: "0")
        var values = [Digit](count: Real.BF_num_values, repeatedValue: 0)
        var zeros = 0
        let point = NSLocale.currentLocale().objectForKey(NSLocaleDecimalSeparator) as! String
        
        // Handle the "not-a-number" case
        if !bf_is_valid {
            mantissaOut = "NaN"
            exponentOut = ""
            return
        }
        
		// Limit the length of the output string
		if lengthLimit > Real.BF_num_values * Int(bf_value_precision) {
			lengthLimit = Real.BF_num_values * Int(bf_value_precision)
		}
		if lengthLimit < 2 {
			// Leave at least room for 2 digits and a decimal point
			lengthLimit = 2
		}
		if (places > lengthLimit - 1) {
			places = lengthLimit - 1;
		}
		
        // Trace through the number looking the the most significant non-zero digit
        var digitsInNumber = self.mantissaLength
		
        // Copy the values
        Real.BF_CopyValues(bf_array, destination: &values)
        var exponentCopy = bf_exponent;
		var userPointCopy = Int(bf_user_point)
        
        // Ensure that we don't have too many leading zeros
        if userPointCopy + 2 > lengthLimit + digitsInNumber {
            exponentCopy -= (userPointCopy - (lengthLimit + digitsInNumber)) + 2
            userPointCopy -= (userPointCopy - (lengthLimit + digitsInNumber)) + 2
            
            if (exponentCopy < 0 && userPointCopy >= digitsInNumber) {
                exponentCopy -= userPointCopy - digitsInNumber + 1
                userPointCopy -= userPointCopy - digitsInNumber + 1
            }
        }
        
        // Handle a fixed number of decimal places
		if places != 0 {
            exponentCopy += places - userPointCopy
            userPointCopy = places
            
            // If there is not enough room to display the number in the current fixed precision, bail out
            if digitsInNumber + exponentCopy > lengthLimit {
                mantissaOut = "∞"
                exponentOut = ""
                return
            }
            
            // Result is zero
            if digitsInNumber + exponentCopy <= 0 || digitsInNumber == 0 {
                var d = 0
                
                digits[0] = "0"
                while d < point.count() {
                    digits[1 + d] = point[d]
                    d++
                }
                for i in 1+d..<places+2 {
                    digits[i] = "0"
                }
                
				mantissaOut = String(digits[0..<places+2])
                exponentOut = ""
                return
            }
            
            // Too many digits so strip them back
			var carryBits : Digit = 0
            while exponentCopy < 0 {
                carryBits = Real.BF_RemoveDigitFromMantissa(&values, radix: bf_radix, limit: bf_value_limit)
                exponentCopy++
                digitsInNumber--
            }
            
            // Apply round to nearest
            if Double(carryBits) >= (Double(bf_radix) / 2.0) {
                Real.BF_AddToMantissa(&values, digit: 1, limit: bf_value_limit)
                
                // In the incredibly unlikely case that this rounding increases the number of digits
                // in the number past the precision, then bail out.
                if (values[Real.BF_num_values - 1] / bf_value_limit != 0) {
					mantissaOut = "∞"
                    exponentOut = ""
                    return
                }
            }
            
            // Not enough digits so pad them out
            while (exponentCopy > 0) {
                Real.BF_AppendDigitToMantissa(&values, digit: 0, radix: bf_radix, limit: bf_value_limit)
                exponentCopy--
                digitsInNumber++
            }
            
            if digitsInNumber > lengthLimit {
				mantissaOut = "∞"
                exponentOut = ""
                return
            }
        } else if digitsInNumber == 0 {
            // If there are no non-zero digits, return a zero string
            mantissaOut = "0"
            exponentOut = self.exponentStringFromInt(exponentCopy)
            return
		} else if (digitsInNumber > lengthLimit || (userPointCopy + 1 > lengthLimit)) {
			// If we have more digits than we can display, truncate the values
			var carryBits : Digit = 0
			while digitsInNumber > lengthLimit || (userPointCopy + 1 > lengthLimit) {
				carryBits = Real.BF_RemoveDigitFromMantissa(&values, radix: bf_radix, limit: bf_value_limit)
				
				digitsInNumber--
				if userPointCopy > 0 {
					userPointCopy--
				} else {
					exponentCopy++
				}
			}
			
            // Apply round to nearest
			if Double(carryBits) >= (Double(bf_radix) / 2.0) {
				Real.BF_AddToMantissa(&values, digit: 1, limit: bf_value_limit)
				
				// If by shear fluke that cause the top digit to overflow, then shift back by one digit
				if (values[Real.BF_num_values - 1] / bf_value_limit != 0) {
					Real.BF_RemoveDigitFromMantissa(&values, radix: bf_radix, limit: bf_value_limit)
					
					if (userPointCopy > 0) {
						userPointCopy--;
					} else {
						exponentCopy++
					}
				}
				
                // We may have changed the number of digits... recount
                digitsInNumber = Int(Real.BF_NumDigitsInArray(values, radix: bf_radix, precision: bf_value_precision))
            }
        }
        
        // Scientific notation weirdisms
        if fill && places == 0 {
            let diff = (digitsInNumber - 1) - userPointCopy
            userPointCopy += diff
            exponentCopy += diff
            
            // Not enough digits so pad them out
            while (digitsInNumber < lengthLimit) {
                Real.BF_AppendDigitToMantissa(&values, digit: 0, radix: bf_radix, limit: bf_value_limit)
                digitsInNumber++
                userPointCopy++
            }
        }
        
        // Handle stuff related to negative numbers
        var currentChar = 0  // digits
        if complement > 0 {
			var complementNumber: Real
			var mantissaNumber: Real
            let complementBits = UInt64(1) << UInt64(complement - 1)
            
			complementNumber = Real(mantissa: complementBits, exponent: 0, isNegative: false, radix: bf_radix, userPointAt: 0)
            mantissaNumber = complementNumber
            Real.BF_CopyValues(mantissaNumber.bf_array, destination: &values)
            
			var carryBits : Digit = 0
			while (
				(mantissaNumber.compareWith(complementNumber) == .OrderedDescending  ||
					(mantissaNumber.compareWith(complementNumber) == .OrderedSame  && !bf_is_negative)) && !mantissaNumber.isZero
				)
			{
				carryBits = Real.BF_RemoveDigitFromMantissa(&mantissaNumber.bf_array, radix: bf_radix, limit: bf_value_limit)
				
				if (userPointCopy > 0) {
					userPointCopy--;
				} else {
					exponentCopy++
				}
			}
            // Apply round to nearest
            if (Double(carryBits) >= (Double(bf_radix) / 2.0)) {
                Real.BF_AddToMantissa(&mantissaNumber.bf_array, digit: 1, limit: bf_value_limit)
                
                // If by shear fluke that cause the top digit to overflow, then shift back by one digit
                if (values[Real.BF_num_values - 1] / bf_value_limit != 0) {
                    Real.BF_RemoveDigitFromMantissa(&mantissaNumber.bf_array, radix: bf_radix, limit: bf_value_limit)
                    
					if (userPointCopy > 0) {
						userPointCopy--;
					} else {
						exponentCopy++
					}
                }
            }
            if (mantissaNumber.compareWith(complementNumber) == .OrderedDescending ||
               (mantissaNumber.compareWith(complementNumber) == .OrderedSame && !bf_is_negative))
            {
				mantissaOut = "∞"
                exponentOut = ""
                return
            }
            
            if bf_is_negative {
                complementNumber = complementNumber.multiplyBy(two)
                complementNumber = complementNumber.subtract(mantissaNumber)
                Real.BF_CopyValues(complementNumber.bf_array, destination: &values)
                digitsInNumber = complementNumber.mantissaLength
            } else {
                Real.BF_CopyValues(mantissaNumber.bf_array, destination: &values)
                digitsInNumber = mantissaNumber.mantissaLength
            }
            
        } else if bf_is_negative {
            digits[currentChar] = "-"
            currentChar++
        }
        
        // Write any leading zeros to the string
        if userPointCopy >= digitsInNumber {
            digits[currentChar] = "0"
            currentChar++
            
            if (userPointCopy - digitsInNumber > 0) {
                var d = 0
                
                while d < point.count() {
                    digits[currentChar++] = point[d]
                    d++
                }
            }
            
            for _ in 0..<userPointCopy - digitsInNumber {
                digits[currentChar] = "0"
                currentChar++
            }
        }
        
        // Write the digits out to the string
        digitsInNumber--
        while digitsInNumber >= 0 {
            let power: Double = Real.pow(bf_radix, digitsInNumber % Int(bf_value_precision))
            let nextDigit = Real.BF_digits[(Int(Double(values[digitsInNumber / Int(bf_value_precision)]) / power) % bf_radix)]
            
            if userPointCopy <= digitsInNumber {
                if userPointCopy != 0 && userPointCopy == (digitsInNumber + 1) {
                    var d = 0
                    while d < point.count() {
                        digits[currentChar++] = point[d]
                        d++
                    }
                }
                
                digits[currentChar] = nextDigit
                currentChar++
            } else if (nextDigit == "0" && !fill && complement == 0 && userPointCopy > digitsInNumber) {
                zeros++
            } else {
                if (userPointCopy != 0 && userPointCopy == (digitsInNumber + 1 + zeros)) {
                    var d = 0
					while d < point.count() {
						digits[currentChar++] = point[d]
						d++
					}
                }
				
                for _ in 0..<zeros {
                    digits[currentChar] = "0"
                    currentChar++
                }
                digits[currentChar] = nextDigit
                currentChar++
                zeros = 0
            }
            
            digitsInNumber--
        }
        
        mantissaOut = String(digits[0..<currentChar])
        exponentOut = self.exponentStringFromInt(exponentCopy)
    }
    
    
    static func Test () {
        // Basic tests of the functionality
        let a = Real(123456.78e10)
        let a1 = BigFloat(double: 123456.78e10, radix: 10)
        let b = Real("-1234567890.12345678901234567890e1000")
        let b1 = BigFloat(string: "-1234567890.12345678901234567890e1000", radix: 10)
        print("a  = \(a), b  = \(b)")
        print("a1 = \(a1), b1 = \(b1)")
        print("a+b   = \(a+b)")
        let c = BigFloat(); c.assign(a1); c.add(b1)
        print("a1+b1 = \(c)")
        print("a-b   = \(a-b)")
        c.assign(a1); c.subtract(b1)
        print("a1-b1 = \(c)")
        print("a*b   = \(a*b)")
        c.assign(a1); c.multiplyBy(b1)
        print("a1*b1 = \(c)")
        print("a/b   = \(a/b)")
        c.assign(a1); c.divideBy(b1)
        print("a1/b1 = \(c)")
        print("a%b   = \(a%b)")
        c.assign(a1); c.moduloBy(b1)
        print("a1%b1 = \(c)")
        let one = Real(1)
        let one1 = BigFloat(int: 1, radix: 10)
        one1.powerOfE()
        print("exp1(1) = \(one1)")
        let e = one.powerOfE()
        print("exp(1)  = \(e)")
        let two = Real(2)
        let c32 = Real(32)
        print("sqrt(2) = \(two.sqrt())")
        print("ln(e) = \(e.ln())")
        print("2**32 = \(two.raiseToPower(c32))")
        print("2**32.5 = \(two.raiseToPower(c32+Real(0.5)))")
        print("32! = \(c32.factorial())")
		print("pi = \(two.pi)")
    }
	
}

extension BigFloat {
    
    // Just needed for Real.Test()
    override public var description : String {
        return self.toString
    }
    
}