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
    
	var isValid: Bool { return bf_is_valid }
		
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
		if abs(bf_exponent) > Real.BF_max_exponent {
			bf_is_valid = false
		}
	}
	
	//
	// True if the number is empty.
	//
	var isZero: Bool {
		return !Real.BF_ArrayIsNonZero(bf_array)
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
	
	//
	// Appends a new digit to this BigFloat. As though the next digit had been typed
	// into the calculator. 2's complement numbers have some weird stuff that needs
	// to be worked through once the digit is appended.
	//
	func appendDigit(digit: Character, useComplement complement:Int) -> Bool {
		//	unsigned long		values[BF_num_values];
		//
		//	if (digit == '-')
		//	{
		//	bf_is_negative = (bf_is_negative == NO) ? YES : NO;
		//	}
		//	else if (digit >= 0 && digit <= 36) // append a regular digit
		//	{
		//	// Do nothing if overflow could occur
		//	if (bf_array[BF_num_values - 1] >= (bf_value_limit / bf_radix))
		//	return NO;
		//
		//	BF_CopyValues(bf_array, values);
		//
		//	// Multiply through by the bf_radix and add the digit
		//	BF_AppendDigitToMantissa(values, digit, bf_radix, bf_value_limit, 1);
		//
		//	if (complement)
		//	{
		//	BigFloat			*complementNumberFull;
		//	BigFloat			*complementNumberHalf;
		//	BigFloat			*mantissaNumber;
		//	unsigned long long	complementHalf = ((unsigned long long)1 << (complement - 1));
		//	unsigned long long	complementFull = ((unsigned long long)1 << (complement));
		//	NSComparisonResult	relative;
		//
		//	complementNumberHalf = [[BigFloat alloc] initWithMantissa:complementHalf exponent:0 isNegative:0 radix:bf_radix userPointAt:0];
		//
		//	if (complement == 64)
		//	{
		//	complementNumberFull = [complementNumberHalf copy];
		//	BigFloat *two = [[BigFloat alloc] initWithInt:2 radix:bf_radix];
		//	[complementNumberFull multiplyBy:two];
		//	}
		//	else
		//	{
		//	complementNumberFull = [[BigFloat alloc] initWithMantissa:complementFull exponent:0 isNegative:0 radix:bf_radix userPointAt:0];
		//	}
		//
		//	mantissaNumber = [complementNumberHalf copy];
		//	BF_AssignValues(mantissaNumber->bf_array, values);
		//
		//	if (!bf_is_negative)
		//	{
		//	relative = [mantissaNumber compareWith:complementNumberHalf];
		//
		//	if (relative == NSOrderedDescending || relative == NSOrderedSame)
		//	{
		//	if  ([mantissaNumber compareWith:complementNumberFull] == NSOrderedAscending)
		//	{
		//	[complementNumberFull subtract:mantissaNumber];
		//	BF_AssignValues(bf_array, complementNumberFull->bf_array);
		//	if (bf_user_point != 0)
		//	bf_user_point++;
		//	bf_is_negative = YES;
		//
		//	return YES;
		//	}
		//
		//	// Overflow, don't apply digit
		//	return NO;
		//	}
		//	}
		//	else
		//	{
		//	// Overflow, don't apply digit
		//	return NO;
		//	}
		//
		//	}
		//
		//	BF_AssignValues(bf_array, values);
		//	
		//	// Move the decimal point along with the digits
		//	if (bf_user_point != 0)
		//	bf_user_point++;
		//	}
		
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
			if abs(bf_exponent) > Int32(pow(Double(bf_radix), Double(bf_exponent_precision)) / Double(bf_radix)) {
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
	
	
}