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
    static let BF_num_values            =	16   // number of digit limbs
	static let BF_digit_bits			=   16
	static let BF_max_radix				=   1 << BF_digit_bits
    static let BF_max_mantissa_length	=	BF_num_values * BF_digit_bits + 3
    static let BF_max_exponent_length	=	sizeof(Int32)*8
	static let BF_max_exponent			=   Int32(Int16.max)
	
	// A string containing the unichar digits 0 to 9 and onwards
	static let BF_digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    
    // Mode for trigonometric operations
    enum BFTrigMode { case BF_degrees, BF_radians, BF_gradians }
    
    typealias Digit = UInt32
    
    private var bf_array = [Digit](count: Real.BF_num_values, repeatedValue: 0)
    private var bf_exponent: Int32 = 0
    private var bf_user_point: UInt16 = 0
    private var bf_is_negative: Bool = false
    
    private var bf_radix: UInt8 = 10
    private var bf_value_precision: UInt32 = 0
    private var bf_value_limit: UInt32 = 0
    private var bf_exponent_precision: UInt32 = 0
	private var bf_is_valid: Bool = false
		
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
        for i in 0..<destination.count {
            destination[i] = source[i]
        }
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
    static func BF_NumDigitsInArray(values: [Digit], radix: UInt8, precision: Digit) -> Int {
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
        
        thisNum.bf_exponent -= Int32(thisNum.bf_user_point)
        thisNum.bf_user_point = 0
        otherNum.bf_exponent -= Int32(otherNum.bf_user_point)
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
	private mutating func setElements(var radix: UInt8, negative isNegative:Bool, exp exponent:Int32, valid isValid:Bool, var userPoint:UInt16) {
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
		bf_value_limit = Digit(pow(rradix, Double(bf_value_precision)))
		bf_exponent_precision = Digit(log(rmax) / log(rradix))
		
		// Apply the decimal point
		if Int(userPoint) > Int(bf_value_precision) * Real.BF_num_values - 1 {
			userPoint = UInt16(Int(bf_value_precision) * Real.BF_num_values - 1)
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
				bf_user_point = UInt16(bf_value_precision * Digit(Real.BF_num_values)) - 1
			} else {
				bf_user_point = UInt16(-bf_exponent)
				bf_exponent = 0
			}
		}
		
		// Standard check on the exponent
		if Swift.abs(bf_exponent) > Real.BF_max_exponent {
			bf_is_valid = false
		}
	}

    // MARK: - Contructors

	//
	// Hey look, its the default constructor. Bet you've never seen one of these before.
	// By default you get a base 10 zero.
	//
	init() {
		Real.BF_ClearValuesArray(&bf_array)
		setElements(10, negative:false, exp:0, valid:true, userPoint:0)
	}

	//
	// Allows fairly explicit contruction of a Real
	//
	init(var mantissa: UInt64, exponent exp: Int32, isNegative flag: Bool, radix newRadix: UInt8, userPointAt pointLocation: UInt16) {
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
	init (var int newValue: Int, radix newRadix: UInt8) {
		let negative = newValue < 0
		if negative { newValue = -newValue }
		
		self.init(mantissa: UInt64(newValue), exponent: 0, isNegative: negative, radix: newRadix, userPointAt: 0)
	}
	
	//
	// Also good but not as fast as initWithInt.
	//
	init(var double
		newValue: Double, radix newRadix:UInt8) {
		var mantissa : UInt64 = 0
		var newExponent: Int32
		var numDigits = 0
		var negative = false
		
		// Shortcut
		if newValue == 0.0 {
			self.init(int:0, radix:newRadix); return
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
			newExponent = Int32(floor(doubleExponent))
		} else {
			newExponent = Int32(ceil(doubleExponent))
		}
		
		// Remove the exponent from the newValue
		newValue /= pow(Double(newRadix), Double(newExponent))
		if newValue.isNaN {
			// Generate an NaN and return
			self.init(int: 0, radix: newRadix)
			bf_is_valid = false
			return
		}
		
		// Get the digits out one at a time, up to the max precision for a double's mantissa
		var intPart = 0.0
		var fracPart: Double
		var nextDigit: Int
		for i in 0..<Int(Double(Int(radixValuePrecision) * sizeof(Double)/sizeof(UInt32)) * 0.8) {
			// The next digit should be the only thing left of the decimal point
			fracPart = modf(newValue, &intPart)
			nextDigit = Int(intPart)
			
			// Only add the digit if it is non-zero
			if (nextDigit != 0) {
				// Guard against overflow
				if UInt64.max / UInt64(newRadix) >= mantissa {
					mantissa = mantissa * UInt64(pow(Double(newRadix), Double(i - numDigits + 1))) + UInt64(nextDigit)
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
			bf_exponent -= Int32(bf_user_point) - Int32(numDigits) + 1
			bf_user_point -= bf_user_point - UInt16(numDigits) + 1
		}
	}
	
//	//
//	// initPiWithRadix
//	//
//	// Creates a number and initialises it to π. At one point I was going to have more of these.
//	// Like a zero, a one and other such numbers. Oh well.
//	//
//	- (instancetype)initPiWithRadix:(unsigned short)newRadix
//	{
//	self = [self initWithInt:0 radix:newRadix];
//	
//	if (self != nil)
//	{
//	// Don't actually return our private PI (in case the caller messes it up)
//	[self assign:self.pi];   // automatically calculate new pi for unknown radices
//	}
//	}
//	
//	//
//	// initWithCoder
//	//
//	// Part of the NSCoder protocol. Required for copy, paste and other such stuff.
//	//
//	- (instancetype)initWithCoder:(NSCoder *)coder
//	{
//	unsigned long	*values;
//	NSUInteger		length;
//	
//	self = [super init];
//	
//	values = (unsigned long *)[coder decodeBytesForKey:@"BFArray" returnedLength:&length];
//	NSAssert(length == sizeof(unsigned long)*BF_num_values, @"Value array is wrong length");
//	BF_AssignValues(bf_array, values);
//	
//	bf_exponent = [coder decodeIntForKey:@"BFExponent"];
//	bf_user_point = [coder decodeIntForKey:@"BFUserPoint"];
//	bf_is_negative = [coder decodeBoolForKey:@"BFIsNegative"];
//	bf_radix = [coder decodeIntForKey:@"BFRadix"];
//	bf_value_precision = [coder decodeIntForKey:@"BFValuePrecision"];
//	bf_value_limit = [coder decodeIntForKey:@"BFValueLimit"];
//	bf_exponent_precision = [coder decodeIntForKey:@"BFExponentPrecision"];
//	bf_is_valid = [coder decodeBoolForKey:@"BFIsValid"];
//	
//	return self;
//	}
	
	//
	// The most requested constructor for those numbers that don't fit in 19 digits.
	// BTW: I'll go with the Swift convention and use a 'p' exponent for radices other than 10.
	//
	init(string newValue: String, radix newRadix:UInt8) {
		
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
		
		self.init(int:0, radix:newRadix)
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
    
    var isInteger: Bool {
        let tmp = self.abs().fractionalPart()
        return tmp.isZero
    }
    
    //
    // Reports what the current fractional point's location is.
    //
    var userPoint: Int { return Int(bf_user_point) }
    
    //
    // Returns the number of digits in the number.
    //
    var mantissaLength: Int {
        return Int(Real.BF_NumDigitsInArray(bf_array, radix: bf_radix, precision: Digit(bf_value_precision)))
    }
    
    //
    // Returns the radix of the current number
    //
    var radix : Int { return Int(bf_radix) }
    
    //
    // Returns whether or not this number is valid (overflow or divide by zero make numbers
    // invalid).
    //
    var isValid: Bool { return bf_is_valid }
    
    //
    // Returns the sign of the current number.
    //
    var isNegative: Bool { return bf_is_negative }
    
    //
    // Returns the presence of an exponent.
    //
    var hasExponent: Bool { return bf_exponent != 0 }
    
    //
    // True if the number is empty.
    //
    var isZero: Bool {
        return !Real.BF_ArrayIsNonZero(bf_array)
    }
    
    //
    // Sets the sign of the number to positive.
    //
    func abs() -> Real {
        var temp = self
        temp.bf_is_negative = false
        return temp
    }
    
    //
    // Sets the sign of the number to positive.
    //
    func negate() -> Real {
        var temp = self
        temp.bf_is_negative = !bf_is_negative
        return temp
    }
	
	//
	// Appends a new digit to this BigFloat. As though the next digit had been typed
	// into the calculator. 2's complement numbers have some weird stuff that needs
	// to be worked through once the digit is appended.
	//
	mutating func appendDigit(digit: Character, useComplement complement:Int) -> Bool {
		var values = [Digit](count: bf_array.count, repeatedValue: 0)
		
		if digit == "-" {
			bf_is_negative = (bf_is_negative == false) ? true : false
		} else if (digit >= "0" && digit <= "9") || (digit >= "A" && digit <= "Z") { // append a regular digit
			// Do nothing if overflow could occur
			if bf_array[Real.BF_num_values - 1] >= (bf_value_limit / UInt32(bf_radix)) {
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
					let two = Real(int: 2, radix: bf_radix)
					complementNumberFull = complementNumberHalf.multiplyBy(two)
				} else {
					complementNumberFull = Real(mantissa: complementFull, exponent: 0, isNegative: false, radix: bf_radix, userPointAt: 0)
				}
				
				mantissaNumber = complementNumberHalf
				Real.BF_CopyValues(mantissaNumber.bf_array, destination: &values)
				
				if !bf_is_negative {
					let relative = mantissaNumber.compareWith(complementNumberHalf)
					
					if (relative == .OrderedDescending || relative == .OrderedSame) {
						if mantissaNumber.compareWith(complementNumberFull) == .OrderedAscending {
							complementNumberFull = complementNumberFull.subtract(mantissaNumber)
							Real.BF_CopyValues(bf_array, destination: &complementNumberFull.bf_array);
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
			
			Real.BF_CopyValues(bf_array, destination: &values)
			
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
	mutating func appendExpDigit (digit: Character) {
		// Change the sign when '+/-' is pressed
		if digit == "-" {
			bf_exponent = -bf_exponent
			return
		}
		
		// Do the appending stuff
		if digit >= "0" && digit <= "9" {
			// Do nothing if overflow could occur
			if Swift.abs(bf_exponent) > Int32(pow(Double(bf_radix), Double(bf_exponent_precision)) / Double(bf_radix)) {
				return
			}
			
			bf_exponent = bf_exponent * Int32(bf_radix) + Int32(String(digit))!
		}
	}
	
	//
	// Puts a fractional point into the number
	//
	mutating func setUserPoint(pointLocation: Int) {
		setElements(bf_radix, negative:bf_is_negative, exp:bf_exponent, valid:isValid, userPoint:UInt16(pointLocation))
	}
    
    func convertToRadix(newRadix: UInt8) -> Real {
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
        let exponent = bf_exponent - Int32(bf_user_point)
        values.bf_user_point = 0

        // Adjust the precision related elements
        let rradix = Double(Real.BF_max_radix)
        values.bf_radix = newRadix
        values.bf_exponent = 0
        values.bf_exponent_precision =  UInt32(log(rradix) / log(Double(values.bf_radix)))
        values.bf_value_precision = UInt32(log(rradix) / log(Double(values.bf_radix)))
        values.bf_value_limit = UInt32(pow(Double(newRadix), Double(values.bf_value_precision)))
        
        // Clear the working space
        var reversed = [Digit](count:self.bf_array.count*2, repeatedValue:0)
        var result = [Digit](count:self.bf_array.count*2, repeatedValue:0)
        
        // Re-encode the mantissa
        for _ in 0..<(Int(values.bf_value_precision) * Real.BF_num_values * 2) {
            // Remove new digits from the old number by integer dividing by the new radix
            let carryBits = Real.BF_RemoveDigitFromMantissa(&values.bf_array, radix: newRadix, limit: bf_value_limit)
            
            // Put all the digits in the new number
            Real.BF_AppendDigitToMantissa(&reversed, digit: carryBits, radix: newRadix, limit: values.bf_value_limit)
        }
        
        // Which is fine, except that all the digits are now reversed
        for _ in 0..<(Int(values.bf_value_precision) * Real.BF_num_values * 2) {
            // Take out backwards
            let carryBits = Real.BF_RemoveDigitFromMantissa(&reversed, radix: newRadix, limit: values.bf_value_limit)
            
            // And put in forwards
            Real.BF_AppendDigitToMantissa(&result, digit: carryBits, radix: newRadix, limit: values.bf_value_limit)
        }
        
        // if result is too big, truncate until it fits into the allowed space
        while result[Real.BF_num_values] > 0 {
            Real.BF_RemoveDigitFromMantissa(&result, radix: newRadix, limit: values.bf_value_limit)
            values.bf_exponent++
        }
        
        // Create a Real with bf_radix = newRadix and value = oldRadix
        var exponentNum = Real(int: Int(bf_radix), radix: newRadix)
        
        // Raise the BigFloat to the old exponent power
        exponentNum = exponentNum.raiseToIntPower(Int(exponent))
        
        // Set the values and elements of this number
        Real.BF_CopyValues(result, destination: &values.bf_array)
        
        // multiply this number by the BigFloat
        return values.multiplyBy(exponentNum)
    }
	
	// MARK: - Arithmetic Functions
    
    //
    // Takes the third root of the receiver
    //
    func raiseToIntPower(n: Int) -> Real {
        var Z = Real(int: 0, radix: bf_radix)
        var N = Swift.abs(n)
        var Y = Real(int: 1, radix: bf_radix)
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
            return Y.inverse()
        }
        return Y
    }
	
	//
	// I have two apples (one of them used to be yours). I eat yours. How many apples do
	// I have now?.
	//
	func subtract(num: Real) -> Real {
		return num
//	int					i, peek;
//	unsigned long		values[BF_num_values];
//	unsigned long		otherNum[BF_num_values];
//	BigFloatElements	thisNumElements;
//	BigFloatElements	otherNumElements;
//	NSComparisonResult	compare;
//	
//	if ([num radix] != bf_radix)
//	{
//	num = [num copy];
//	[num convertToRadix:bf_radix];
//	}
//	
//	[self copyElements: &thisNumElements];
//	[num copyElements: &otherNumElements];
//	
//	// ignore invalid numbers
//	if (otherNum.bf_is_valid == NO || thisNumElements.bf_is_valid == NO)
//	{
//	bf_is_valid = NO;
//	return;
//	}
//	
//	// Handle differences in sign by calling addition instead
//	if (otherNum.bf_is_negative != thisNumElements.bf_is_negative)
//	{
//	bf_is_negative = !bf_is_negative;
//	[self add: num];
//	bf_is_negative = !bf_is_negative;
//	return;
//	}
//	
//	BF_CopyValues(bf_array, values);
//	BF_CopyValues(num->bf_array, otherNum);
//	
//	BF_NormaliseNumbers(values, otherNum, &thisNumElements, &otherNumElements);
//	
//	// Compare the two values
//	compare = NSOrderedSame;
//	for (i = BF_num_values - 1; i >= 0; i--)
//	{
//	if (values[i] > otherNum[i])
//	{
//	compare = NSOrderedDescending;
//	break;
//	}
//	else if (values[i] < otherNum[i])
//	{
//	compare = NSOrderedAscending;
//	break;
//	}
//	}
//	
//	if (compare == NSOrderedDescending)
//	{
//	// Perform the subtraction
//	for (i = 0; i < BF_num_values; i++)
//	{
//	// Borrow from the next column if we need to
//	if (otherNum[i] > values[i])
//	{
//	// Since we know that this num is greater than otherNum, then we know
//	// that this will never exceed the bounds of the array
//	peek = 1;
//	while(values[i + peek] == 0)
//	{
//	values[i + peek] = thisNumElements.bf_value_limit - 1;
//	peek++;
//	}
//	values[i+peek]--;
//	values[i] += thisNumElements.bf_value_limit;
//	}
//	values[i] = values[i] - otherNum[i];
//	}
//	}
//	else if (compare == NSOrderedAscending)
//	{
//	// Change the sign of this num
//	thisNumElements.bf_is_negative = !thisNumElements.bf_is_negative;
//	
//	// Perform the subtraction
//	for (i = 0; i < BF_num_values; i++)
//	{
//	// Borrow from the next column if we need to
//	if (values[i] > otherNum[i])
//	{
//	// Since we know that this num is greater than otherNum, then we know
//	// that this will never exceed the bounds of the array
//	peek = 1;
//	while(otherNum[i + peek] == 0)
//	{
//	otherNum[i + peek] = otherNum.bf_value_limit - 1;
//	peek++;
//	}
//	otherNum[i+peek]--;
//	otherNum[i] += otherNum.bf_value_limit;
//	}
//	values[i] = otherNum[i] - values[i];
//	}
//	}
//	else
//	{
//	// Zero the exponent and remove the sign
//	thisNumElements.bf_exponent = 0;
//	thisNumElements.bf_is_negative = NO;
//	
//	// Subtraction results in zero
//	BF_ClearValuesArray(values, 1);
//	}
//	
//	// Create a user pont, store all the values back in the class and we're done
//	BF_AssignValues(bf_array, values);
//	[self assignElements: &thisNumElements];
//	[self createUserPoint];
	
	}
	
	//
	// multiplyBy
	//
	// I take the 8 seeds out of my apple. I plant them in the ground and grow 8 trees.
	// Each tree has 8 apples, how successful is my orchard?
	//
	func multiplyBy(num: Real) -> Real {
//	int					i, j;
//	long				carryBits = 0;
//	unsigned long		result[BF_num_values * 2];
//	unsigned long		values[BF_num_values];
//	unsigned long		otherNum[BF_num_values];
//	BigFloatElements	thisNumElements;
//	BigFloatElements	otherNumElements;
//	BOOL				shift = NO;
//	
//	if ([num radix] != bf_radix)
//	{
//	num = [num copy];
//	[num convertToRadix:bf_radix];
//	}
//	
//	// Get a working copy of the values that will be multiplied
//	BF_CopyValues(bf_array, values);
//	[self copyElements: &thisNumElements];
//	BF_CopyValues(num->bf_array, otherNum);
//	[num copyElements: &otherNumElements];
//	
//	// ignore invalid numbers
//	if (otherNum.bf_is_valid == NO || thisNumElements.bf_is_valid == NO)
//	{
//	bf_is_valid = NO;
//	return;
//	}
//	
//	// Apply the user's decimal point
//	thisNumElements.bf_exponent -=thisNumElements. bf_user_point;
//	thisNumElements.bf_user_point = 0;
//	otherNum.bf_exponent -= otherNum.bf_user_point;
//	otherNum.bf_user_point = 0;
//	
//	// Multiply exponents through addition
//	thisNumElements.bf_exponent += otherNum.bf_exponent;
//	
//	// Two negatives make a positive
//	if (otherNum.bf_is_negative) (thisNumElements.bf_is_negative) ? (thisNumElements.bf_is_negative = NO) : (thisNumElements.bf_is_negative = YES);
//	
//	// Clear the result space
//	BF_ClearValuesArray(result, 2);
//	
//	// Now we do the multiplication. Basic stuff:
//	// Multiply each column of each of the otherNums by each other and sum all of the results
//	for (j = 0; j < BF_num_values; j++)
//	{
//	// Add the product of this column of otherNum with this num
//	carryBits = 0;
//	for (i = 0; i < BF_num_values; i++)
//	{
//	result[i + j] += (values[i] * otherNum[j]) + carryBits;
//	carryBits = result[i + j] / thisNumElements.bf_value_limit;
//	result[i + j] = result[i + j] % thisNumElements.bf_value_limit;
//	
//	if (i + j >= BF_num_values && result[i + j] != 0) shift = YES;
//	}
//	
//	// Add the carry for the last multiplication to the next column
//	result[j + BF_num_values] += carryBits;
//	if (result[j + BF_num_values] != 0) shift = YES;
//	}
//	
//	// If we have exceeded the precision, divide by the bf_radix until
//	// we are reeled back in.
//	while(BF_ArrayIsNonZero(&result[BF_num_values], 1))
//	{
//	carryBits = BF_RemoveDigitFromMantissa(result, thisNumElements.bf_radix, thisNumElements.bf_value_limit, 2);
//	thisNumElements.bf_exponent++;
//	}
//	
//	// Apply round to nearest
//	if ((double)carryBits >= ((double)bf_radix / 2.0))
//	{
//	BF_AddToMantissa(result, 1, thisNumElements.bf_value_limit, 1);
//	
//	// If by shear fluke that caused the top digit to overflow, then shift back by one digit
//	if (result[BF_num_values - 1] > thisNumElements.bf_value_limit)
//	{
//	BF_RemoveDigitFromMantissa(result, thisNumElements.bf_radix, thisNumElements.bf_value_limit, 1);
//	thisNumElements.bf_exponent++;
//	}
//	}
//	
//	// Create a user pont, store all the values back in the class and we're done
//	BF_AssignValues(bf_array, result);
//	[self assignElements: &thisNumElements];
//	[self createUserPoint];
		return num
	}
    
    //
    // divideBy
    //
    // You see my orchard and get jealous. You start a fire that takes out half my crop.
    // How do you explain your actions to the judge?
    //
    func divideBy(num: Real) -> Real {
        return self
//    int					i, j, peek;
//    unsigned long		carryBits;
//    unsigned long		values[BF_num_values * 2];
//    unsigned long		otherNumValues[BF_num_values * 2];
//    unsigned long		result[BF_num_values * 2];
//    unsigned long		subValues[BF_num_values * 2];
//    BigFloatElements	thisNumElements;
//    BigFloatElements	otherNumElements;
//    unsigned long		quotient;
//    NSComparisonResult	compare;
//    
//    if ([num radix] != bf_radix)
//    {
//    num = [num copy];
//    [num convertToRadix:bf_radix];
//    }
//    
//    // Clear the working space
//    BF_ClearValuesArray(otherNumValues, 1);
//    BF_ClearValuesArray(values, 1);
//    BF_ClearValuesArray(result, 2);
//    BF_ClearValuesArray(subValues, 2);
//    
//    // Get the numerical values
//    BF_CopyValues(bf_array, &values[BF_num_values]);
//    [self copyElements: &thisNumElements];
//    BF_CopyValues(num->bf_array, &otherNumValues[BF_num_values]);
//    [num copyElements: &otherNumElements];
//    
//    // ignore invalid numbers
//    if (otherNumElements.bf_is_valid == NO || thisNumElements.bf_is_valid == NO)
//    {
//    bf_is_valid = NO;
//    return;
//    }
//    
//    // Apply the user's decimal point
//    thisNumElements.bf_exponent -= thisNumElements.bf_user_point;
//    thisNumElements.bf_user_point = 0;
//    otherNumElements.bf_exponent -= otherNumElements.bf_user_point;
//    otherNumElements.bf_user_point = 0;
//    
//    // Two negatives make a positive
//    if (otherNumElements.bf_is_negative) (thisNumElements.bf_is_negative) ? (thisNumElements.bf_is_negative = NO) : (thisNumElements.bf_is_negative = YES);
//    
//    // Normalise this num
//    // This involves multiplying through by the bf_radix until the number runs up against the
//    // left edge or MSD (most significant digit)
//    if (BF_ArrayIsNonZero(values, 2))
//    {
//    while(values[BF_num_values * 2 - 1] < (thisNumElements.bf_value_limit / thisNumElements.bf_radix))
//    {
//    BF_AppendDigitToMantissa(values, 0, thisNumElements.bf_radix, thisNumElements.bf_value_limit, 2);
//    
//    thisNumElements.bf_exponent--;
//    }
//    }
//    else
//    {
//    BF_AssignValues(bf_array, &values[BF_num_values]);
//    bf_exponent = 0;
//    bf_user_point = 0;
//    bf_is_negative = 0;
//    
//    if (!BF_ArrayIsNonZero(otherNumValues, 2))
//    {
//    bf_is_valid = NO;
//    }
//    
//    return;
//    }
//    
//    // We have the situation where otherNum had a larger kNumValue'th digit than
//    // this num did in the first place. So we may have to divide through by bf_radix
//    // once to normalise otherNum
//    if (otherNumValues[BF_num_values * 2 - 1] > values[BF_num_values * 2 - 1])
//    {
//    carryBits = BF_RemoveDigitFromMantissa(otherNumValues, thisNumElements.bf_radix, thisNumElements.bf_value_limit, 2);
//    otherNumElements.bf_exponent++;
//    
//    if ((double)carryBits >= ((double)otherNumElements.bf_radix / 2.0))
//    {
//    BF_AddToMantissa(otherNumValues, 1, otherNumElements.bf_value_limit, 2);
//    }
//    }
//    else
//    {
//    // Normalise otherNum so that it cannot be greater than this num
//    // This involves multiplying through by the bf_radix until the number runs up
//    // against the left edge or MSD (most significant digit)
//    // If the last multiply will make otherNum greater than this num, then we
//    // don't do it. This ensures that the first division column will always be non-zero.
//    if (BF_ArrayIsNonZero(otherNumValues, 2))
//    {
//    while
//    (
//				(otherNumValues[BF_num_values * 2 - 1] < (otherNumElements.bf_value_limit / otherNumElements.bf_radix))
//				&&
//				(otherNumValues[BF_num_values * 2 - 1] < (values[BF_num_values * 2 - 1] / otherNumElements.bf_radix))
//    )
//    {
//				BF_AppendDigitToMantissa(otherNumValues, 0, thisNumElements.bf_radix, thisNumElements.bf_value_limit, 2);
//				otherNumElements.bf_exponent--;
//    }
//    }
//    else
//    {
//    bf_is_valid = NO;
//    return;
//    }
//    }
//    
//    // Subtract the exponents
//    thisNumElements.bf_exponent -= otherNumElements.bf_exponent;
//    
//    // Account for the de-normalising effect of division
//    thisNumElements.bf_exponent -= (BF_num_values - 1) * thisNumElements.bf_value_precision;
//    
//    // Begin the division
//    // What we are doing here is lining the divisor up under the divisee and subtracting the largest multiple
//    // of the divisor that we can from the divisee with resulting in a negative number. Basically it is what
//    // you do without really thinking about it when doing long division by hand.
//    for (i = BF_num_values * 2 - 1; i >= BF_num_values - 1; i--)
//    {
//    // If the divisor is greater or equal to the divisee, leave this result column unchanged.
//    if (otherNumValues[BF_num_values * 2 - 1] > values[i])
//    {
//    if (i > 0)
//    {
//				values[i - 1] += values[i] * thisNumElements.bf_value_limit;
//    }
//    continue;
//    }
//    
//    // Determine the quotient of this position (the multiple of  the divisor to use)
//    quotient = values[i] / otherNumValues[BF_num_values * 2 - 1];
//    carryBits = 0;
//    for (j = 0; j <= i; j++)
//    {
//    subValues[j] = otherNumValues[j + (BF_num_values * 2 - 1 - i)] * quotient + carryBits;
//    carryBits = subValues[j] / thisNumElements.bf_value_limit;
//    subValues[j] %= thisNumElements.bf_value_limit;
//    }
//    subValues[i] += carryBits * thisNumElements.bf_value_limit;
//    
//    // Check that values is greater than subValues (ie check that this subtraction won't
//    // result in a negative number)
//    compare = NSOrderedSame;
//    for (j = i; j >= 0; j--)
//    {
//    if (values[j] > subValues[j])
//    {
//				compare = NSOrderedDescending;
//				break;
//    }
//    else if (values[j] < subValues[j])
//    {
//				compare = NSOrderedAscending;
//				break;
//    }
//    }
//    
//    // If we have overestimated the quotient, adjust appropriately. This just means that we need
//    // to reduce the divisor's multiplier by one.
//    while(compare == NSOrderedAscending)
//    {
//    quotient--;
//    carryBits = 0;
//    for (j = 0; j <= i; j++)
//    {
//				subValues[j] = otherNumValues[j + (BF_num_values * 2 - 1 - i)] * quotient + carryBits;
//				carryBits = subValues[j] / thisNumElements.bf_value_limit;
//				subValues[j] %= thisNumElements.bf_value_limit;
//    }
//    subValues[i] += carryBits * thisNumElements.bf_value_limit;
//    
//    // Check that values is greater than subValues (ie check that this subtraction won't
//    // result in a negative number)
//    compare = NSOrderedSame;
//    for (j = i; j >= 0; j--)
//    {
//				if (values[j] > subValues[j])
//				{
//    compare = NSOrderedDescending;
//    break;
//				}
//				else if (values[j] < subValues[j])
//				{
//    compare = NSOrderedAscending;
//    break;
//				}
//    }
//    }
//    
//    // We now have the number to place in this column of the result. Yay.
//    result[i] = quotient;
//    
//    // If the subtraction operation will result in no remainder, then finish
//    if (compare == NSOrderedSame)
//    {
//    break;
//    }
//    
//    // Subtract the sub values from values now
//    for (j = (BF_num_values * 2 - 1); j >= 0; j--)
//    {
//    if (subValues[j] > values[j])
//    {
//				// Since we know that this num is greater than the sub num, then we know
//				// that this will never exceed the bounds of the array
//				peek = 1;
//				while(values[j + peek] == 0)
//				{
//    values[j + peek] = thisNumElements.bf_value_limit - 1;
//    peek++;
//				}
//				values[j+peek]--;
//				values[j] += thisNumElements.bf_value_limit;
//    }
//    values[j] -= subValues[j];
//    }
//    
//    // Attach the remainder to the next column on the right so that it will be part of the next
//    // column's operation
//    values[i - 1] += values[i] * thisNumElements.bf_value_limit;
//    
//    // Clear the remainder from this column
//    values[i] = 0;
//    subValues[i] = 0;
//    }
//    
//    // Normalise the result
//    // This involves multiplying through by the bf_radix until the number runs up against the
//    // left edge or MSD (most significant digit)
//    while(result[BF_num_values * 2 - 1] < (thisNumElements.bf_value_limit / thisNumElements.bf_radix))
//    {
//    BF_AppendDigitToMantissa(result, 0, thisNumElements.bf_radix, thisNumElements.bf_value_limit, 2);
//    thisNumElements.bf_exponent--;
//    }
//    
//    // Apply a round to nearest on the last digit
//    if (((double)result[BF_num_values - 1] / (double)(bf_value_limit / bf_radix)) >= ((double)bf_radix / 2.0))
//    {
//    BF_AddToMantissa(&result[BF_num_values], 1, thisNumElements.bf_value_limit, 1);
//    
//    // If by shear fluke that cause the top digit to overflow, then shift back by one digit
//    if (result[BF_num_values - 1] > thisNumElements.bf_value_limit)
//    {
//    carryBits = BF_RemoveDigitFromMantissa(&result[BF_num_values], thisNumElements.bf_radix, thisNumElements.bf_value_limit, 1);
//    thisNumElements.bf_exponent++;
//    if ((double)carryBits >= ((double)thisNumElements.bf_radix / 2.0))
//    {
//				BF_AddToMantissa(&result[BF_num_values], 1, thisNumElements.bf_value_limit, 1);
//    }
//    }
//    }
//    
//    // Remove any trailing zeros in the decimal places by dividing by the bf_radix until they go away
//    carryBits = 0;
//    while((thisNumElements.bf_exponent < 0) && (result[BF_num_values] % thisNumElements.bf_radix == 0))
//    {
//    carryBits = BF_RemoveDigitFromMantissa(&result[BF_num_values], thisNumElements.bf_radix, thisNumElements.bf_value_limit, 1);
//    thisNumElements.bf_exponent++;
//    }
//    if ((double)carryBits >= ((double)thisNumElements.bf_radix / 2.0))
//    {
//    BF_AddToMantissa(&result[BF_num_values], 1, thisNumElements.bf_value_limit, 1);
//    }
//    
//    
//    // Create a user pont, store all the values back in the class and we're done
//    BF_AssignValues(bf_array, &result[BF_num_values]);
//    [self assignElements: &thisNumElements];
//    [self createUserPoint];
    
    }
    
    //
    // moduloBy
    //
    // The judge orders that that the orchard be divided between you, me and the judge. The
    // remaining tree is given to charity.
    //
    func moduloBy(num: Real) -> Real {
        return self
//    int					i, j, peek;
//    unsigned long		carryBits;
//    unsigned long		values[BF_num_values * 2];
//    unsigned long		otherNumValues[BF_num_values * 2];
//    unsigned long		result[BF_num_values * 2];
//    unsigned long		subValues[BF_num_values * 2];
//    BigFloatElements	otherNumElements;
//    BigFloatElements	thisNumElements;
//    unsigned long		quotient;
//    NSComparisonResult	compare;
//    int					divisionExponent;
//    BigFloat			*subNum;
//    
//    if ([num radix] != bf_radix)
//    {
//    num = [num copy];
//    [num convertToRadix:bf_radix];
//    }
//    
//    // Clear the working space
//    BF_ClearValuesArray(otherNumValues, 1);
//    BF_ClearValuesArray(values, 1);
//    BF_ClearValuesArray(result, 2);
//    BF_ClearValuesArray(subValues, 2);
//    
//    // Get the numerical values
//    BF_CopyValues(bf_array, &values[BF_num_values]);
//    [self copyElements: &thisNumElements];
//    BF_CopyValues(num->bf_array, &otherNumValues[BF_num_values]);
//    [num copyElements: &otherNumElements];
//    
//    // ignore invalid numbers
//    if (otherNumElements.bf_is_valid == NO || thisNumElements.bf_is_valid == NO)
//    {
//    bf_is_valid = NO;
//    return;
//    }
//    
//    compare = [self compareWith: num];
//    if (compare == NSOrderedAscending)
//    {
//    // return unchanged if num is less than the modulor
//    return;
//    }
//    
//    // Apply the user's decimal point
//    thisNumElements.bf_exponent -= thisNumElements.bf_user_point;
//    thisNumElements.bf_user_point = 0;
//    otherNumElements.bf_exponent -= otherNumElements.bf_user_point;
//    otherNumElements.bf_user_point = 0;
//    
//    // Two negatives make a positive
//    if (otherNumElements.bf_is_negative) (thisNumElements.bf_is_negative) ? (thisNumElements.bf_is_negative = NO) : (thisNumElements.bf_is_negative = YES);
//    
//    // Normalise this num
//    // This involves multiplying through by the bf_radix until the number runs up against the
//    // left edge or MSD (most significant digit)
//    if (BF_ArrayIsNonZero(values, 2))
//    {
//    while(values[BF_num_values * 2 - 1] < (thisNumElements.bf_value_limit / thisNumElements.bf_radix))
//    {
//    BF_AppendDigitToMantissa(values, 0, thisNumElements.bf_radix, thisNumElements.bf_value_limit, 2);
//    
//    thisNumElements.bf_exponent--;
//    }
//    }
//    else
//    {
//    BF_AssignValues(bf_array, &values[BF_num_values]);
//    bf_exponent = 0;
//    bf_user_point = 0;
//    bf_is_negative = 0;
//    return;
//    }
//    
//    // Normalise otherNum so that it cannot be greater than this num
//    // This involves multiplying through by the bf_radix until the number runs up
//    // against the left edge or MSD (most significant digit)
//    // If the last multiply will make otherNum greater than this num, then we
//    // don't do it. This ensures that the first division column will always be non-zero.
//    if (BF_ArrayIsNonZero(otherNumValues, 2))
//    {
//    while
//    (
//    (otherNumValues[BF_num_values * 2 - 1] < (otherNumElements.bf_value_limit / otherNumElements.bf_radix))
//    &&
//    (otherNumValues[BF_num_values * 2 - 1] < (values[BF_num_values * 2 - 1] / otherNumElements.bf_radix))
//    )
//    {
//    BF_AppendDigitToMantissa(otherNumValues, 0, thisNumElements.bf_radix, thisNumElements.bf_value_limit, 2);
//    otherNumElements.bf_exponent--;
//    }
//    }
//    else
//    {
//    bf_is_valid = NO;
//    return;
//    }
//    
//    // Subtract the exponents
//    divisionExponent = thisNumElements.bf_exponent - otherNumElements.bf_exponent;
//    
//    // Account for the de-normalising effect of division
//    divisionExponent -= (BF_num_values - 1) * thisNumElements.bf_value_precision;
//    
//    // Set the re-normalised values so that we can subtract from self later
//    BF_AssignValues(bf_array, &values[BF_num_values]);
//    [self assignElements: &thisNumElements];
//    
//    // Begin the division
//    // What we are doing here is lining the divisor up under the divisee and subtracting the largest multiple
//    // of the divisor that we can from the divisee with resulting in a negative number. Basically it is what
//    // you do without really thinking about it when doing long division by hand.
//    for (i = BF_num_values * 2 - 1; i >= BF_num_values - 1; i--)
//    {
//    // If the divisor is greater or equal to the divisee, leave this result column unchanged.
//    if (otherNumValues[BF_num_values * 2 - 1] > values[i])
//    {
//    if (i > 0)
//    {
//				values[i - 1] += values[i] * thisNumElements.bf_value_limit;
//    }
//    continue;
//    }
//    
//    // Determine the quotient of this position (the multiple of  the divisor to use)
//    quotient = values[i] / otherNumValues[BF_num_values * 2 - 1];
//    carryBits = 0;
//    for (j = 0; j <= i; j++)
//    {
//    subValues[j] = otherNumValues[j + (BF_num_values * 2 - 1 - i)] * quotient + carryBits;
//    carryBits = subValues[j] / bf_value_limit;
//    subValues[j] %= bf_value_limit;
//    }
//    subValues[i] += carryBits * bf_value_limit;
//    
//    // Check that values is greater than subValues (ie check that this subtraction won't
//    // result in a negative number)
//    compare = NSOrderedSame;
//    for (j = i; j >= 0; j--)
//    {
//    if (values[j] > subValues[j])
//    {
//				compare = NSOrderedDescending;
//				break;
//    }
//    else if (values[j] < subValues[j])
//    {
//				compare = NSOrderedAscending;
//				break;
//    }
//    }
//    
//    // If we have overestimated the quotient, adjust appropriately. This just means that we need
//    // to reduce the divisor's multiplier by one.
//    while(compare == NSOrderedAscending)
//    {
//    quotient--;
//    carryBits = 0;
//    for (j = 0; j <= i; j++)
//    {
//				subValues[j] = otherNumValues[j + (BF_num_values * 2 - 1 - i)] * quotient + carryBits;
//				carryBits = subValues[j] / thisNumElements.bf_value_limit;
//				subValues[j] %= thisNumElements.bf_value_limit;
//    }
//    subValues[i] += carryBits * thisNumElements.bf_value_limit;
//    
//    // Check that values is greater than subValues (ie check that this subtraction won't
//    // result in a negative number)
//    compare = NSOrderedSame;
//    for (j = i; j >= 0; j--)
//    {
//				if (values[j] > subValues[j])
//				{
//    compare = NSOrderedDescending;
//    break;
//				}
//				else if (values[j] < subValues[j])
//				{
//    compare = NSOrderedAscending;
//    break;
//				}
//    }
//    }
//    
//    // We now have the number to place in this column of the result. Yay.
//    result[i] = quotient;
//    
//    // If the subtraction operation will result in no remainder, then finish
//    if (compare == NSOrderedSame)
//    {
//    break;
//    }
//    
//    // Subtract the sub values from values now
//    for (j = 0; j < BF_num_values * 2; j++)
//    {
//    if (subValues[j] > values[j])
//    {
//				// Since we know that this num is greater than the sub num, then we know
//				// that this will never exceed the bounds of the array
//				peek = 1;
//				while(values[j + peek] == 0)
//				{
//    values[j + peek] = bf_value_limit - 1;
//    peek++;
//				}
//				values[j + peek]--;
//				values[j] += bf_value_limit;
//    }
//    values[j] -= subValues[j];
//    }
//    
//    // Attach the remainder to the next column on the right so that it will be part of the next
//    // column's operation
//    values[i - 1] += values[i] * bf_value_limit;
//    
//    // Clear the remainder from this column
//    values[i] = 0;
//    subValues[i] = 0;
//    }
//    
//    // Remove the fractional part of the division result
//    // We know that there must be a non-fractional part since the modulor was tested to
//    // be less or equal to the modulee
//    while(divisionExponent < 0)
//    {
//    carryBits = BF_RemoveDigitFromMantissa(&result[BF_num_values], thisNumElements.bf_radix, thisNumElements.bf_value_limit, 1);
//    result[BF_num_values - 1] += carryBits * thisNumElements.bf_value_limit;
//    carryBits = BF_RemoveDigitFromMantissa(result, thisNumElements.bf_radix, thisNumElements.bf_value_limit, 1);
//    divisionExponent++;
//    }
//    
//    // Now create a number that is this dividend times the modulor and subtract it from the
//    // modulee to obtain the result
//    subNum = [[BigFloat alloc] initWithInt:0 radix:thisNumElements.bf_radix];
//    [subNum setElements:thisNumElements.bf_radix negative:thisNumElements.bf_is_negative exp:divisionExponent valid:YES userPoint:0];
//    BF_CopyValues(&result[BF_num_values], subNum->bf_array);
//    
//    [subNum multiplyBy: num];
//    [self subtract: subNum];
//    
//    // Remove any trailing zeros in the decimal places by dividing by the bf_radix until they go away
//    BF_CopyValues(bf_array, values);
//    [self copyElements: &thisNumElements];
//    while((thisNumElements.bf_exponent < 0) && (values[0] % thisNumElements.bf_radix == 0))
//    {
//    carryBits = BF_RemoveDigitFromMantissa(values, thisNumElements.bf_radix, thisNumElements.bf_value_limit, 1);
//    bf_exponent++;
//    }
//    
//    // Create a user pont, store all the values back in the class and we're done
//    BF_AssignValues(bf_array, values);
//    [self assignElements: &thisNumElements];
//    [self createUserPoint];
    }

	
	//
	// Returns the scale of the receiver with respect to num.
	//
	func compareWith(num: Real) -> NSComparisonResult {
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
    // Performs 1/receiver.
    //
    func inverse() -> Real {
        var inverseValue: Real
        
        if !bf_is_valid { return self }
        
        if isZero {
            inverseValue = self
            inverseValue.bf_is_valid = false
            return inverseValue
        }
        inverseValue = Real(int: 1, radix: bf_radix)
        return inverseValue.divideBy(self)
    }
    
    //
    // fractionalPart
    //
    //
    // Sets the receiver to the receiver modulo 1.
    //
    func fractionalPart() -> Real {
        if !bf_is_valid { return self}
        
        let one = Real(int:1, radix:bf_radix)
        return self.moduloBy(one)
    }
    
    //
    // wholePart
    //
    // Sets the receiver to (receiver - (receiver modulo 1))
    //
    func wholePart() -> Real {
        if !bf_is_valid { return self }
        
        let isNegative = bf_is_negative
        let numb = self.abs()

        var whole = numb.subtract(numb.fractionalPart())
        whole.bf_is_negative = isNegative
        return whole
    }

	
}