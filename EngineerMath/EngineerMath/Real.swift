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
//  Precision is defined by NumDigits. It defines how many UInt32 are
//  used to hold the number. Though in reality, only half of each UInt32 is used. This
//  is so that when you multiply them together, there is room for the result  (16 bits
//  multiplied by 16 bits requires all 32 bits). If you really wanted to, you could
//  change this class so that NumDigits was chosen at class initialisation time
//
//  Based on BigFloat.m from the Magic Number Machine
//  Created by Matt Gallagher on Sun Jan 06 2002.
//  Copyright © 2002-2003 Matt Gallagher. All rights reserved.
//

import Foundation

// SignedNumberType compliance
public func == (lhs: Real, rhs: Real) -> Bool { return lhs.compareWith(rhs) == .OrderedSame }
public func < (lhs: Real, rhs: Real) -> Bool { return lhs.compareWith(rhs) == .OrderedAscending }
public prefix func - (z:Real) -> Real { return z.negate() }

public prefix func + (z:Real) -> Real { return z }
public func + (lhs: Real, rhs: Real) -> Real { return lhs.add(rhs) }
public func += (inout lhs: Real, rhs: Real) { lhs = lhs + rhs }
public func - (lhs: Real, rhs: Real) -> Real { return lhs.subtract(rhs) }
public func -= (inout lhs: Real, rhs: Real) { lhs = lhs - rhs }
public func * (lhs: Real, rhs: Real) -> Real { return lhs.multiplyBy(rhs) }
public func *= (inout lhs: Real, rhs: Real) { lhs = lhs * rhs }
public func / (lhs: Real, rhs: Real) -> Real { return lhs.divideBy(rhs) }
public func /= (inout lhs: Real, rhs: Real) { lhs = lhs / rhs }
public func % (lhs: Real, rhs: Real) -> Real { return lhs.moduloBy(rhs) }
public func %= (inout lhs: Real, rhs: Real) { lhs = lhs % rhs }

public func & (lhs: Real, rhs: Real) -> Real { return lhs.andWith(rhs, usingComplement: 0) }
public func &= (inout lhs: Real, rhs: Real) { lhs = lhs & rhs }
public func | (lhs: Real, rhs: Real) -> Real { return lhs.orWith(rhs, usingComplement: 0) }
public func |= (inout lhs: Real, rhs: Real) { lhs = lhs | rhs }
public func ^ (lhs: Real, rhs: Real) -> Real { return lhs.xorWith(rhs, usingComplement: 0) }
public func ^= (inout lhs: Real, rhs: Real) { lhs = lhs ^ rhs }
public prefix func ~ (z:Real) -> Real { return z.notUsingComplement(0) }

public protocol BasicOperationType : SignedNumberType {
    func +(lhs: Self, rhs: Self) -> Self
    func -(lhs: Self, rhs: Self) -> Self
    func *(lhs: Self, rhs: Self) -> Self
    func /(lhs: Self, rhs: Self) -> Self
}

extension Int : BasicOperationType { }  // used for ipower generic instance of Int

public struct Real : CustomStringConvertible, BasicOperationType {

    // Basic constants defining the precision used by the class
    static let NumDigits            = 16                  // number of digit limbs
	static let BitsPerDigit			= sizeof(Digit)*8/2   // bits per digit limb
	static let MaxRadix				= 1 << BitsPerDigit
    static let LogMaxRadix          = Foundation.log(Double(MaxRadix))
    static let MaxMantissaLength	= NumDigits * BitsPerDigit + 3
    static let MaxExponentLength	= sizeof(Int32)*8
	static let MaxExponent			= 0xFFFF
	
	// A string containing the unichar digits 0 to 9 and onwards
	static let ValidDigits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    
    // Mode for trigonometric operations
    public enum TrigMode { case degrees, radians, gradians }
    
    typealias Digit = UInt32
    
    private var digits = [Digit](count: Real.NumDigits, repeatedValue: 0)
    private var exponent: Int = 0
    private var userPoint: Int = 0
	private var valid: FloatingPointClassification = .QuietNaN
    private var radix: Int = 10 {
        didSet {
            // update related variables
            let realRadix = Double(radix)
            valuePrecision = Digit(Real.LogMaxRadix / Foundation.log(realRadix))
            valueLimit = Digit(Real.ipower(radix, n: Int(valuePrecision)))
        }
    }
	private var negative: Bool {
		get {
			return valid == FloatingPointClassification.NegativeNormal
		}
		set {
			if newValue {
				// set negative status in valid
				if valid == .PositiveNormal {
					valid = .NegativeNormal
				}
			} else {
				if valid == .NegativeNormal {
					valid = .PositiveNormal
				}
			}
		}
	}
    
    // these are recalculated automatically when radix changes
    private var valuePrecision: Digit = 0
    private var valueLimit: Digit = 0
		
    // MARK: - Helper Functions
    
    //
    // Sets every value in a values array to zero
    //
    static func ClearValuesArray(inout values: [Digit]) {
        // Set the value to zero
        values = [Digit](count: values.count, repeatedValue: 0)
    }
    
    //
    // Scans a values array looking for any non-zero digits.
    //
    static func ArrayIsNonZero(values: [Digit], start: Int = 0) -> Bool {
        for i in start..<values.count {
            if values[i] != 0 { return true }
        }
        
        return false
    }
    
    //
    // Copies the source values to the destination values.
    //
    static func CopyValues(source: [Digit], inout destination: [Digit], sstart: Int = 0, dstart: Int = 0) {
        // Do a basic copy of the values into the destination
        for i in 0..<NumDigits {
            destination[i+dstart] = source[i+sstart]
        }
    }
    
    static func AssignValues(inout destination: [Digit], source: [Digit], sstart: Int = 0, dstart: Int = 0) {
        CopyValues(source, destination: &destination, sstart: sstart, dstart: dstart)
    }
    
    //
    // Adds a single UInt32 to an array of values.
    //
    static func AddToMantissa(inout values: [Digit], var digit: Digit, limit: Digit, start: Int = 0, scale: Int = 1) {
        for i in start..<start+NumDigits*scale {
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
    static func AppendDigitToMantissa(inout values: [Digit], var digit: Digit, radix: Int, limit: Digit, scale: Int = 1) {
        // Multiply through by the radix and add the digit
        for i in 0..<NumDigits * scale {
            values[i] = (values[i] * Digit(radix)) + digit
            digit	  = values[i] / limit
            values[i] = values[i] % limit
        }
        values[values.count - 1] += digit * limit
    }
    
    //
    // Chops a single digit off the end of the values array by dividing through by the radix.
    //
    static func RemoveDigitFromMantissa(inout values: [Digit], radix: Int, limit: Digit, start: Int = 0, scale: Int = 1) -> Digit  {
        // Truncate a digit by dividing through by the radix
        var carryBits : Digit = 0
        
        for i in (start..<start+NumDigits*scale).reverse() {
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
    static func RemoveDigitFromMantissaAndFlagEmpty(inout values: [Digit], radix: Int, limit: Digit, inout isEmpty: Bool) -> Digit {
        // Truncate a digit by dividing through by the radix
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
    static func NumDigitsInArray(values: [Digit], radix: Int, precision: Digit) -> Int {
        var digitsInNumber, valueNumber, digitNumber: Int
        
        // Trace through the number looking the the most significant non-zero digit
        digitsInNumber =  NumDigits * Int(precision)
        valueNumber = NumDigits
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
    static func NormaliseNumbers(inout thisNum: Real, inout otherNum: Real) {
        assert(otherNum.radix == thisNum.radix, "Numbers must have same radix before normalisation")
        
        var thisRoundingNum : Digit = 0
        var otherRoundingNum : Digit = 0
        var thisEmpty = false
        var otherEmpty = false
        
        thisNum.exponent -= thisNum.userPoint
        thisNum.userPoint = 0
        otherNum.exponent -= otherNum.userPoint
        otherNum.userPoint = 0
        
        // Normalise due to otherNum.exponent being greater than exponent
        if otherNum.exponent > thisNum.exponent {
            // start by normalising otherNum left
            while otherNum.exponent > thisNum.exponent &&
                  otherNum.digits[NumDigits - 1] < (otherNum.valueLimit / Digit(otherNum.radix))
            {
                AppendDigitToMantissa(&otherNum.digits, digit: 0, radix: otherNum.radix, limit: otherNum.valueLimit)
                otherNum.exponent--
            }
            
            // then normalise this num to the right
            while otherNum.exponent > thisNum.exponent && !thisEmpty {
                thisRoundingNum = RemoveDigitFromMantissaAndFlagEmpty(&thisNum.digits, radix: thisNum.radix, limit: thisNum.valueLimit, isEmpty: &thisEmpty)
                thisNum.exponent++
            }
        }
            
        // Normalise due to this exponent being greater than otherNum->exponent
        else if thisNum.exponent > otherNum.exponent {
            // start by normalising this num left
            while thisNum.exponent > otherNum.exponent &&
                  thisNum.digits[NumDigits - 1] < (thisNum.valueLimit / Digit(thisNum.radix))
            {
                AppendDigitToMantissa(&thisNum.digits, digit: 0, radix: thisNum.radix, limit: thisNum.valueLimit)
                thisNum.exponent--
            }
            // then normalise otherNum to the right
            while thisNum.exponent > otherNum.exponent && !otherEmpty {
                otherRoundingNum = RemoveDigitFromMantissaAndFlagEmpty(&otherNum.digits, radix: otherNum.radix, limit: otherNum.valueLimit, isEmpty: &otherEmpty)
                otherNum.exponent++
            }
        }
        
        // Apply a round to nearest on any truncated values
        if (!otherEmpty && Double(otherRoundingNum) >= (Double(thisNum.radix) / 2.0)) {
            AddToMantissa(&otherNum.digits, digit: 1, limit: otherNum.valueLimit)
        } else if (!thisEmpty && Double(thisRoundingNum) >= (Double(thisNum.radix) / 2.0)) {
            AddToMantissa(&thisNum.digits, digit: 1, limit: thisNum.valueLimit)
        }
        
        if thisEmpty && !otherEmpty {
            thisNum.exponent = otherNum.exponent
        } else if !thisEmpty && otherEmpty {
            otherNum.exponent = thisNum.exponent
        } else if thisEmpty && otherEmpty {
            otherNum.exponent = 0
            thisNum.exponent = 0
        }
    }
	
	// MARK: - Private utility functions
	
	//
	// Allows the elements of a Real to be safely set.
	//
	private mutating func setElements(radix: Int, negative isNegative:Bool, exp exponent:Int, valid isValid:Bool, userPoint:Int) {
		// Set everything
		self.exponent = exponent
		valid = isValid ? isNegative ? .NegativeNormal: .PositiveNormal : .QuietNaN
		
		// Set the radix (if it is valid), and update valuePrecision and valueLimit
        self.radix = radix < 2 || radix > 36 ? 10 : radix
		
		// Apply the decimal point
        let limit = Int(valuePrecision * Digit(Real.NumDigits) - 1)
		self.userPoint = min(userPoint, limit)
	}
	
	//
	// Puts a fractional point in a number according to typical expected behaviour.
	//
	private mutating func createUserPoint() {
		if isZero {
			exponent = 0
			userPoint = 0
			return
		}
		
		// Extract a user decimal point (because 45.67 is prettier than 4567e-2)
		if exponent < 0 {
			if Digit(-exponent) > valuePrecision * Digit(Real.NumDigits) {
				exponent += Int32(valuePrecision * Digit(Real.NumDigits)) - 1
				userPoint = Int(valuePrecision * Digit(Real.NumDigits)) - 1
			} else {
				userPoint = -exponent
				exponent = 0
			}
		}
		
		// Standard check on the exponent
		if Swift.abs(exponent) > Real.MaxExponent {
			valid = .QuietNaN
		}
	}
    
    public func toRadiansFrom(mode: TrigMode) -> Real {
        var result = self
        if mode != .radians {
            if mode == .degrees {
                let oneEighty = Real(180, radix: radix)
                let threeSixty = Real(360, radix: radix)
                result = (self / oneEighty) % threeSixty
            } else if mode == .gradians {
                let twoHundred = Real(200, radix: radix)
                let fourHundred = Real(400, radix: radix)
                result = (self / twoHundred) % fourHundred
            }
            return result * π
        } else {
            let two_pi = two * π
            return self % two_pi
        }
    }
    
    public func radiansToMode(mode: TrigMode) -> Real {
        var result = self
        if mode != .radians {
            if mode == .degrees {
                let oneEighty = Real(180, radix: radix)
                result = self * oneEighty
            } else if mode == .gradians {
                let twoHundred = Real(200, radix: radix)
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
                let complementNumberHalf = Real(mantissa:complementHalf, exponent:0, isNegative:false, radix:radix, userPointAt:0)
                let complementNumberFull = complementNumberHalf * two
                return complementNumberFull + self
            }
        }
        return self
    }
    
    private func postComplement(complement: Int) -> Real {
        if complement != 0 {
            let complementHalf = UInt64(1) << UInt64(complement - 1)
            let complementNumberHalf = Real(mantissa:complementHalf, exponent:0, isNegative:false, radix:radix, userPointAt:0)
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
        return Digit(Real.ipower(x, n: power))
    }
    
    //
    // returns op(receiver, num)
    //
    private func opWith(num: Real, usingComplement complement: Int, andOp op: (Digit, Digit) -> Digit) -> Real {
        if !isValid { return self }
        if !num.isValid { return num }
        
        var thisNum = self.preComplement(complement)
        var otherNum = num.preComplement(complement)
        
        // Convert to a radix that is a power of 2
        let old_radix = radix
        if radix != 2 && radix != 4 && radix != 8 && radix != 16 && radix != 32 {
            // Convert to the largest possible binary compatible radix
            thisNum = thisNum.convertToRadix(32)
        }
        otherNum = otherNum.convertToRadix(thisNum.radix)
        
        // look for the first digit
        var digit = Real.NumDigits * Int(thisNum.valuePrecision) - 1
        var ind = digit /  Int(thisNum.valuePrecision)
        var offset: Digit = Real.pow(thisNum.radix, digit % Int(thisNum.valuePrecision))
        while offset != 0 && thisNum.digits[ind] / offset == 0 && otherNum.digits[ind] / offset == 0 && digit >= 0 {
            digit--
            ind = digit / Int(thisNum.valuePrecision)
            offset = Real.pow(radix, digit % Int(thisNum.valuePrecision))
        }
        
        // apply a binary op to each digit
        while offset != 0 && digit >= 0 {
            var this_digit = (thisNum.digits[ind] / offset) % Digit(thisNum.radix)
            let that_digit = (otherNum.digits[ind] / offset) % Digit(thisNum.radix)
            thisNum.digits[ind] -= this_digit * Digit(offset)
            this_digit = op(this_digit, that_digit) % Digit(thisNum.radix)
            thisNum.digits[ind] += this_digit * offset
            
            digit--
            ind = digit / Int(thisNum.valuePrecision)
            offset = Real.pow(thisNum.radix, digit % Int(thisNum.valuePrecision))
        }
        
        // Restore the radix
        thisNum = thisNum.convertToRadix(old_radix)
        return thisNum.postComplement(complement)
    }


    // MARK: - Contructors

	//
	// Hey look, it's the default constructor. Bet you've never seen one of these before.
	// By default you get a base 10 zero because we have ten fingers.
	//
	public init() {
		self.init(mantissa: 0, exponent: 0, isNegative: false, radix: 10, userPointAt: 0)
    }

	//
	// Allows fairly explicit contruction of a Real
	//
	public init(var mantissa: UInt64, exponent exp: Int, isNegative flag: Bool, radix newRadix: Int, userPointAt pointLocation: Int) {
		Real.ClearValuesArray(&digits)
		setElements(newRadix, negative:flag, exp:exp, valid:true, userPoint:pointLocation)
		
		// Set the values
		let limit = UInt64(valueLimit)
        for i in 0..<Real.NumDigits where mantissa > 0 {
            digits[i] = Digit(mantissa % limit); mantissa /= limit
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
    // IntegerLiteralConvertible compliance
    //
	public init (integerLiteral value: Int) { self.init(value, radix: 10) }
	
	//
	// Also good but not as fast as initWithInt.
	//
	public init(var _ newValue: Double, radix newRadix: Int = 10) {
		var mantissa : UInt64 = 0
		var newExponent: Int
		var numDigits = 0
		let negative = newValue < 0
		
		// Shortcut
		if newValue == 0 { self.init(0, radix:newRadix); return }
		
		// Determine what the valuePrecision would be for this radix
        let logNewRadix = Foundation.log(Double(newRadix))
		let radixValuePrecision = Digit(Real.LogMaxRadix / logNewRadix)
		
		// Determine the sign
		if negative { newValue = -newValue }
		
		// Get the base radix exponent
		let doubleExponent = Foundation.log(newValue) / logNewRadix
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
			valid = .QuietNaN
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
			while mantissa % UInt64(newRadix) == 0 && numDigits > 1 {
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
		if Int(userPoint) >= numDigits {
			exponent -= Int(userPoint) - numDigits + 1
			userPoint -= userPoint - numDigits + 1
		}
	}
	
	//
	// The most requested constructor for those numbers that don't fit in 19 digits.
	// I'll use the Swift convention and use a 'p' exponent for radices other than 10.
	//
	public init(_ newValue: String, radix newRadix: Int = 10) {
		
		func getCharFromString(inout string: String) -> Character {
			if !string.isEmpty {
				let ch = string.removeAtIndex(string.startIndex)
				return ch
			}
			return "\0"
		}
		
		let separators = newRadix == 10 ? "eE" : "pP"
		let index = advance(Real.ValidDigits.startIndex, Int(newRadix))
		let digits = Real.ValidDigits.substringToIndex(index)
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
				if expNegative { appendExpDigit("-") }
			}
		}
        setElements(radix, negative: isNegative, exp: exponent, valid: isValid, userPoint: userPoint)
	}
	
	// MARK: - Public utility functions
    
    public var isInteger: Bool { return abs.fractionalPart.isZero }
    
    //
    // Calculate π for the current radix and cache it in the array
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
        let quarter 	= Real(0.25, radix: radix)
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
            let a = (one - y.raiseToIntPower(4)).pow(quarter)
            
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
        Real.piArray[Int(radix)] = p.inverse
        return Real.piArray[Int(radix)]!
    }
	
	//
	// I really don't like this but it is convenient.  It seems weird
	// to have a number act like a factory.  Maybe it would be better
    // to return the product of the number and specified factory value?
    // So, to get 2π you would use two.π as the invocation.  Alternatively,
    // if a default radix were specified in the Real class, this could be done
    // properly by having the class act as a factory.
    //
    private static var piArray = [Real?](count: 36, repeatedValue: nil)
    public var pi: Real {
        if let mpi = Real.piArray[Int(radix)] {
            return mpi
        } else {
            return calculatePi()
        }
    }
    public var π: Real { return pi }
	public var zero: Real { return Real(0, radix:radix) }
	public var one: Real { return Real(1, radix:radix) }
	public var two: Real { return Real(2, radix:radix) }
	public var four: Real { return Real(4, radix:radix) }
    
    //
    // Reports and sets the current fractional point's location.
    //
    public var decimalPoint: Int {
        get { return Int(userPoint) }
        set { setElements(radix, negative:isNegative, exp:exponent, valid:isValid, userPoint:newValue) }
    }
    
    //
    // Returns the number of digits in the number.
    //
    public var mantissaLength: Int {
        return Int(Real.NumDigitsInArray(digits, radix: radix, precision: valuePrecision))
    }
	
	public var hashValue: Int {
		var hash = valid.hashValue ^ exponent.hashValue
		for digit in digits { hash ^= digit.hashValue }
		return hash
	}
	
    //
    // Returns the radix or base of the current number
    //
    public var base : Int { return Int(radix) }
    
    //
    // Returns whether or not this number is valid (overflow or divide by zero make numbers
    // invalid).
    //
    public var isValid: Bool { return valid == .PositiveNormal || valid == .NegativeNormal }
	public var floatingPointClass: FloatingPointClassification { return valid }
    
    //
    // Returns the sign of the current number.
    //
    public var isNegative: Bool { return negative }
    
    //
    // Returns the presence of an exponent.
    //
    public var hasExponent: Bool { return exponent != 0 }
    
    //
    // True if the number is empty.
    //
    public var isZero: Bool {
        return !Real.ArrayIsNonZero(digits)
    }
    
    //
    // Sets the sign of the number to positive.
    //
	public var abs: Real {
        var temp = self
        temp.negative = false
        return temp
    }
    
    //
    // Sets the sign of the number to positive.
    //
    public func negate() -> Real {
        var temp = self
        temp.negative = !negative
        return temp
    }
    
    public var epsilon: Real {
        return Real(mantissa: 1, exponent: -(Real.NumDigits * Int(valuePrecision)) + 1, isNegative: false, radix: radix, userPointAt: 0)
    }
	
	//
	// Appends a new digit to this BigFloat. As though the next digit had been typed
	// into the calculator. 2's complement numbers have some weird stuff that needs
	// to be worked through once the digit is appended.
	//
	public mutating func appendDigit(digit: Character, useComplement complement:Int) -> Bool {
		var values = [Digit](count: digits.count, repeatedValue: 0)
		
		if digit == "-" {
			negative = !isNegative ? true : false
		} else if (digit >= "0" && digit <= "9") || (digit >= "A" && digit <= "Z") { // append a regular digit
			// Do nothing if overflow could occur
			if digits[Real.NumDigits - 1] >= (valueLimit / Digit(radix)) {
				return false
			}
			
			Real.CopyValues(digits, destination: &values)
			
			// Multiply through by the radix and add the digit
			let idigit: Digit
			let aChar : Character = "A"
			if digit <= "9" { idigit = Digit(String(digit))! }
			else { idigit = Digit(digit.unicodeValue() - aChar.unicodeValue() + 10) }
			Real.AppendDigitToMantissa(&values, digit: idigit, radix: radix, limit: valueLimit)
			
			if complement != 0 {
				var complementNumberFull: Real
				var complementNumberHalf: Real
				var mantissaNumber: Real
				let complementHalf = UInt64(1) << UInt64(complement - 1)
				let complementFull = UInt64(1) << UInt64(complement)
				
				complementNumberHalf = Real(mantissa: complementHalf, exponent: 0, isNegative: false, radix: radix, userPointAt: 0)
				
				if complement == 64 {
					complementNumberFull = complementNumberHalf.multiplyBy(two)
				} else {
					complementNumberFull = Real(mantissa: complementFull, exponent: 0, isNegative: false, radix: radix, userPointAt: 0)
				}
				
				mantissaNumber = complementNumberHalf
				Real.AssignValues(&mantissaNumber.digits, source: values)
				
				if !isNegative {
					let relative = mantissaNumber.compareWith(complementNumberHalf)
					
					if (relative == .OrderedDescending || relative == .OrderedSame) {
						if mantissaNumber.compareWith(complementNumberFull) == .OrderedAscending {
							complementNumberFull = complementNumberFull.subtract(mantissaNumber)
							Real.AssignValues(&digits, source: complementNumberFull.digits);
							if (userPoint != 0) {
								userPoint++
							}
							negative = true
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
			
			Real.AssignValues(&digits, source: values)
			
			// Move the decimal point along with the digits
			if userPoint != 0 {
				userPoint++
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
			exponent = -exponent
			return
		}
		
		// Do the appending stuff
		if digit >= "0" && digit <= "9" {
			// Do nothing if overflow could occur
            let power: Double = Real.pow(radix, Int(valuePrecision))
			if Swift.abs(exponent) > Int(power / Double(radix)) {
				return
			}
			
			exponent = exponent * radix + Int(String(digit))!
		}
	}
    
    public func convertToRadix(newRadix: Int) -> Real {
        // Get a copy of the relevant stuff
        var values = self
        
        // Check for a valid new radix
        if (radix == newRadix || newRadix < 2 || newRadix > 36) {
            return values
        }
        
        // ignore invalid numbers
        if !isValid {
            values.radix = newRadix
            return values
        }
        
        // Apply the user's decimal point
        let exponent = self.exponent - Int(userPoint)
        values.userPoint = 0

        // Adjust the precision related elements
        values.radix = newRadix  // also calculates valuePrecision & valueLimit
        values.exponent = 0
        
        // Clear the working space
        var reversed = [Digit](count:Real.MaxRadix*2, repeatedValue:0)
        var result = [Digit](count:Real.MaxRadix*2, repeatedValue:0)
        
        // Re-encode the mantissa
        for _ in 0..<(Int(values.valuePrecision) * Real.NumDigits * 2) {
            // Remove new digits from the old number by integer dividing by the new radix
            let carryBits = Real.RemoveDigitFromMantissa(&values.digits, radix: newRadix, limit: valueLimit)
            
            // Put all the digits in the new number
            Real.AppendDigitToMantissa(&reversed, digit: carryBits, radix: newRadix, limit: values.valueLimit, scale:2)
        }
        
        // Which is fine, except that all the digits are now reversed
        for _ in 0..<(Int(values.valuePrecision) * Real.NumDigits * 2) {
            // Take out backwards
            let carryBits = Real.RemoveDigitFromMantissa(&reversed, radix: newRadix, limit: values.valueLimit, scale:2)
            
            // And put in forwards
            Real.AppendDigitToMantissa(&result, digit: carryBits, radix: newRadix, limit: values.valueLimit, scale:2)
        }
        
        // if result is too big, truncate until it fits into the allowed space
        while result[Real.NumDigits] > 0 {
            Real.RemoveDigitFromMantissa(&result, radix: newRadix, limit: values.valueLimit, scale:2)
            values.exponent++
        }
        
        // Create a Real with radix = newRadix and value = oldRadix
        var exponentNum = Real(radix, radix: newRadix)
        
        // Raise the BigFloat to the old exponent power
        exponentNum = exponentNum.raiseToIntPower(exponent)
        
        // Set the values and elements of this number
        Real.AssignValues(&values.digits, source: result)
        
        // multiply this number by the BigFloat
        return values.multiplyBy(exponentNum)
    }
	
	// MARK: - Arithmetic Functions
    
    private static func ipower<T:BasicOperationType>(number: T, power: Int) -> T {
        var Z : T = 0
        var N = Swift.abs(power)
        var Y : T = 1
        
        if number == 0 {
            // Zero raised to anything except zero is zero (provided exponent is valid)
            if power == 0 { return Y }
            return number
        }
        
        Z = number
        while true {
            let t = N & 1; N /= 2
            if t != 0 {
                Y = Y * Z
            }
            if N == 0 { break }
            Z = Z * Z
        }
        if power < 0 {
            return 1 / Y
        }
        return Y
    }
    
    private static func ipower (number: Int, n: Int) -> Int {
        return Real.ipower(number, power: n)
    }
    
    //
    // Takes the receiver to the nth power.
    //
    public func raiseToIntPower(n: Int) -> Real {
        return Real.ipower(self, power: n)
    }
	
	//
	// If I have one apple and you give me another apple, how many apples do I have.
	//
	public func add(num: Real) -> Real {
		var thisNum = self
		var otherNum = num
		
		if num.radix != Int(radix) {
			otherNum = num.convertToRadix(radix)
		}
		
		// ignore invalid numbers
		if !otherNum.isValid || !thisNum.isValid {
			thisNum.valid = thisNum.isValid ? otherNum.valid : thisNum.valid
			return thisNum
		}
		
		// Handle differences in sign by calling subtraction instead
		if otherNum.isNegative != thisNum.isNegative {
			thisNum.negative = !isNegative
			thisNum = thisNum.subtract(num)
			
			if !thisNum.isZero {
				thisNum.negative = !thisNum.negative
			}
			
			return thisNum
		}
		
		Real.NormaliseNumbers(&thisNum, otherNum: &otherNum)
		
		// We can finally do the addition at this point (yay!)
		var carryBits : Digit = 0
		for i in 0..<Real.NumDigits {
			thisNum.digits[i] = thisNum.digits[i] + otherNum.digits[i] + carryBits
			carryBits = thisNum.digits[i] / thisNum.valueLimit
			thisNum.digits[i] %= thisNum.valueLimit
		}
		
		// If we have exceeded the maximum precision, reel it back in
		if carryBits != 0 {
			thisNum.digits[Real.NumDigits - 1] += carryBits * thisNum.valueLimit
			
			carryBits = Real.RemoveDigitFromMantissa(&thisNum.digits, radix: thisNum.radix, limit: thisNum.valueLimit)
			thisNum.exponent++
		}
		
		// Apply round to nearest
		if (Double(carryBits) >= (Double(thisNum.radix) / 2.0)) {
			Real.AddToMantissa(&thisNum.digits, digit: 1, limit: thisNum.valueLimit)
			
			// If by shear fluke that cause the top digit to overflow, then shift back by one digit
			if thisNum.digits[Real.NumDigits - 1] > thisNum.valueLimit {
				Real.RemoveDigitFromMantissa(&thisNum.digits, radix: thisNum.radix, limit: thisNum.valueLimit)
				thisNum.exponent++
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
		
		if num.radix != Int(radix) {
			otherNum = num.convertToRadix(radix)
		}
		
		// ignore invalid numbers
		if !otherNum.isValid || !thisNum.isValid {
			thisNum.valid = thisNum.isValid ? otherNum.valid : thisNum.valid
			return thisNum
		}
		
		// Handle differences in sign by calling addition instead
		if otherNum.isNegative != thisNum.isNegative {
			thisNum.negative = !isNegative
			thisNum = thisNum.add(otherNum)
			thisNum.negative = !thisNum.isNegative
			return thisNum
		}
		
		Real.NormaliseNumbers(&thisNum, otherNum: &otherNum)
		
		// Compare the two values
		var compare = NSComparisonResult.OrderedSame
		for i in (0..<Real.NumDigits).reverse() {
			if thisNum.digits[i] > otherNum.digits[i] {
				compare = .OrderedDescending
				break
			} else if thisNum.digits[i] < otherNum.digits[i] {
				compare = .OrderedAscending
				break
			}
		}
		
		if compare == .OrderedDescending {
			// Perform the subtraction
			for i in 0..<Real.NumDigits {
				// Borrow from the next column if we need to
				if otherNum.digits[i] > thisNum.digits[i] {
					// Since we know that this num is greater than otherNum, then we know
					// that this will never exceed the bounds of the array
					var peek = 1
					while thisNum.digits[i + peek] == 0 {
						thisNum.digits[i + peek] = thisNum.valueLimit - 1
						peek++
					}
					thisNum.digits[i+peek]--
					thisNum.digits[i] += thisNum.valueLimit
				}
				thisNum.digits[i] = thisNum.digits[i] - otherNum.digits[i]
			}
		} else if compare == .OrderedAscending {
			// Change the sign of this num
			thisNum.negative = !thisNum.isNegative
			
			// Perform the subtraction
			for i in 0..<Real.NumDigits {
				// Borrow from the next column if we need to
				if thisNum.digits[i] > otherNum.digits[i] {
					// Since we know that this num is greater than otherNum, then we know
					// that this will never exceed the bounds of the array
					var peek = 1
					while otherNum.digits[i + peek] == 0 {
						otherNum.digits[i + peek] = otherNum.valueLimit - 1
						peek++
					}
					otherNum.digits[i+peek]--
					otherNum.digits[i] += otherNum.valueLimit
				}
				thisNum.digits[i] = otherNum.digits[i] - thisNum.digits[i];
			}
		} else {
			// Zero the exponent and remove the sign
			thisNum.exponent = 0
			thisNum.negative = false
			
			// Subtraction results in zero
			Real.ClearValuesArray(&thisNum.digits)
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
		var result = [Digit](count: Real.NumDigits * 2, repeatedValue: 0)
		
		if num.radix != Int(radix) {
			otherNum = num.convertToRadix(radix)
		}
		
		// ignore invalid numbers
		if !otherNum.isValid || !thisNum.isValid {
			thisNum.valid = thisNum.isValid ? otherNum.valid : thisNum.valid
			return thisNum
		}
		
		// Apply the user's decimal point
		thisNum.exponent -= Int(thisNum.userPoint)
		thisNum.userPoint = 0
		otherNum.exponent -= Int(otherNum.userPoint)
		otherNum.userPoint = 0
		
		// Multiply exponents through addition
		thisNum.exponent += otherNum.exponent;
		
		// Two negatives make a positive
		if otherNum.isNegative { thisNum.negative = thisNum.isNegative ? false : true }
		
		// Now we do the multiplication. Basic stuff:
		// Multiply each column of each of the otherNums by each other and sum all of the results
		var carryBits : Digit = 0
		for j in 0..<Real.NumDigits {
			// Add the product of this column of otherNum with this num
			carryBits = 0
			for i in 0..<Real.NumDigits	{
				result[i + j] += (thisNum.digits[i] * otherNum.digits[j]) + carryBits
				carryBits = result[i + j] / thisNum.valueLimit
				result[i + j] = result[i + j] % thisNum.valueLimit
			}
			
			// Add the carry for the last multiplication to the next column
			result[j + Real.NumDigits] += carryBits
		}
		
		// If we have exceeded the precision, divide by the radix until
		// we are reeled back in.
        while Real.ArrayIsNonZero(result, start:Real.NumDigits) {
			carryBits = Real.RemoveDigitFromMantissa(&result, radix: thisNum.radix, limit: thisNum.valueLimit, scale:2)
			thisNum.exponent++
		}
		
		// Apply round to nearest
		if Double(carryBits) >= Double(radix) / 2.0 {
			Real.AddToMantissa(&result, digit: 1, limit: thisNum.valueLimit)
			
			// If by shear fluke that caused the top digit to overflow, then shift back by one digit
			if (result[Real.NumDigits - 1] > thisNum.valueLimit) {
				Real.RemoveDigitFromMantissa(&result, radix: thisNum.radix, limit: thisNum.valueLimit)
				thisNum.exponent++
			}
		}
		
		// Create a user pont, store all the values back in the class and we're done
		Real.CopyValues(result, destination: &thisNum.digits)
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
		var values = [Digit](count: Real.NumDigits * 2, repeatedValue: 0)
		var otherNumValues = values
		var result = values
		var subValues = values
		var thisNum = self
		var otherNum = num
		
		if num.radix != Int(radix) {
			otherNum = num.convertToRadix(radix)
		}
		
		// Get the numerical values
        Real.CopyValues(digits, destination: &values, dstart: Real.NumDigits)
        Real.CopyValues(otherNum.digits, destination: &otherNumValues, dstart: Real.NumDigits)
		
		// ignore invalid numbers
		if !otherNum.isValid || !thisNum.isValid {
			thisNum.valid = thisNum.isValid ? otherNum.valid : thisNum.valid
			return thisNum
		}
		
		// Apply the user's decimal point
		thisNum.exponent -= Int(thisNum.userPoint)
		thisNum.userPoint = 0
		otherNum.exponent -= Int(otherNum.userPoint)
		otherNum.userPoint = 0
		
		// Two negatives make a positive
		if otherNum.isNegative { thisNum.negative = thisNum.isNegative ? false : true }
		
		// Normalise this num
		// This involves multiplying through by the radix until the number runs up against the
		// left edge or MSD (most significant digit)
        if Real.ArrayIsNonZero(values) {
			while values[Real.NumDigits * 2 - 1] < (thisNum.valueLimit / Digit(thisNum.radix)) {
                Real.AppendDigitToMantissa(&values, digit: 0, radix: thisNum.radix, limit: thisNum.valueLimit, scale:2)
				thisNum.exponent--
			}
		} else {
            Real.AssignValues(&thisNum.digits, source: values, sstart:Real.NumDigits)
			thisNum.exponent = 0
			thisNum.userPoint = 0
			thisNum.negative = false
			
			if !Real.ArrayIsNonZero(otherNumValues) {
				thisNum.valid = .PositiveInfinity
			}
			
			return thisNum
		}
		
		// We have the situation where otherNum had a larger kNumValue'th digit than
		// this num did in the first place. So we may have to divide through by radix
		// once to normalise otherNum
		if (otherNumValues[Real.NumDigits * 2 - 1] > values[Real.NumDigits * 2 - 1]) {
            let carryBits = Real.RemoveDigitFromMantissa(&otherNumValues, radix: thisNum.radix, limit: thisNum.valueLimit, scale:2)
			otherNum.exponent++
			
			if (Double(carryBits) >= (Double(otherNum.radix) / 2.0)) {
				Real.AddToMantissa(&otherNumValues, digit: 1, limit: otherNum.valueLimit, scale:2)
			}
		} else {
			// Normalise otherNum so that it cannot be greater than this num
			// This involves multiplying through by the radix until the number runs up
			// against the left edge or MSD (most significant digit)
			// If the last multiply will make otherNum greater than this num, then we
			// don't do it. This ensures that the first division column will always be non-zero.
			if Real.ArrayIsNonZero(otherNumValues) {
				while (otherNumValues[Real.NumDigits * 2 - 1] < (otherNum.valueLimit / Digit(otherNum.radix))) &&
					  (otherNumValues[Real.NumDigits * 2 - 1] < (values[Real.NumDigits * 2 - 1] / Digit(otherNum.radix)))
				{
					Real.AppendDigitToMantissa(&otherNumValues, digit: 0, radix: thisNum.radix, limit: thisNum.valueLimit, scale:2)
					otherNum.exponent--
				}
			} else {
				thisNum.valid = .PositiveInfinity
				return thisNum
			}
		}
		
		// Subtract the exponents
		thisNum.exponent -= otherNum.exponent;
		
		// Account for the de-normalising effect of division
		thisNum.exponent -= (Real.NumDigits - 1) * Int(thisNum.valuePrecision)
		
		// Begin the division
		// What we are doing here is lining the divisor up under the divisee and subtracting the largest multiple
		// of the divisor that we can from the divisee with resulting in a negative number. Basically it is what
		// you do without really thinking about it when doing long division by hand.
		var carryBits : Digit = 0
		for i in (Real.NumDigits-1..<Real.NumDigits * 2).reverse() {
			// If the divisor is greater or equal to the divisee, leave this result column unchanged.
			if otherNumValues[Real.NumDigits * 2 - 1] > values[i] {
				if i > 0 {
					values[i - 1] += values[i] * thisNum.valueLimit
				}
				continue
			}
			
			// Determine the quotient of this position (the multiple of  the divisor to use)
			var quotient = values[i] / otherNumValues[Real.NumDigits * 2 - 1];
			carryBits = 0
			for j in 0...i {
				subValues[j] = otherNumValues[j + (Real.NumDigits * 2 - 1 - i)] * quotient + carryBits
				carryBits = subValues[j] / thisNum.valueLimit
				subValues[j] %= thisNum.valueLimit
			}
			subValues[i] += carryBits * thisNum.valueLimit
			
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
					subValues[j] = otherNumValues[j + (Real.NumDigits * 2 - 1 - i)] * quotient + carryBits;
					carryBits = subValues[j] / thisNum.valueLimit;
					subValues[j] %= thisNum.valueLimit;
				}
				subValues[i] += carryBits * thisNum.valueLimit;
				
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
			for j in (0..<Real.NumDigits * 2).reverse() {
				if subValues[j] > values[j] {
					// Since we know that this num is greater than the sub num, then we know
					// that this will never exceed the bounds of the array
					var peek = 1
					while values[j + peek] == 0 {
						values[j + peek] = thisNum.valueLimit - 1
						peek++
					}
					values[j+peek]--
					values[j] += thisNum.valueLimit
				}
				values[j] -= subValues[j]
			}
			
			// Attach the remainder to the next column on the right so that it will be part of the next
			// column's operation
			values[i - 1] += values[i] * thisNum.valueLimit
			
			// Clear the remainder from this column
			values[i] = 0
			subValues[i] = 0
		}
		
		// Normalise the result
		// This involves multiplying through by the radix until the number runs up against the
		// left edge or MSD (most significant digit)
		while result[Real.NumDigits * 2 - 1] < (thisNum.valueLimit / Digit(thisNum.radix)) {
			Real.AppendDigitToMantissa(&result, digit: 0, radix: thisNum.radix, limit: thisNum.valueLimit, scale:2)
			thisNum.exponent--
		}
		
		// Apply a round to nearest on the last digit
		if ((Double(result[Real.NumDigits - 1]) / Double(valueLimit / Digit(radix))) >= (Double(radix) / 2.0)) {
            Real.AddToMantissa(&result, digit: 1, limit: thisNum.valueLimit, start: Real.NumDigits)
			
			// If by shear fluke that cause the top digit to overflow, then shift back by one digit
			if (result[Real.NumDigits - 1] > thisNum.valueLimit) {
				carryBits = Real.RemoveDigitFromMantissa(&result, radix: thisNum.radix, limit: thisNum.valueLimit, start: Real.NumDigits)
				thisNum.exponent++
				if Double(carryBits) >= (Double(thisNum.radix) / 2.0) {
					Real.AddToMantissa(&result, digit: 1, limit: thisNum.valueLimit, start: Real.NumDigits)
				}
			}
		}
		
		// Remove any trailing zeros in the decimal places by dividing by the radix until they go away
        carryBits = 0
		while (thisNum.exponent < 0) && (result[Real.NumDigits] % Digit(thisNum.radix) == 0) {
			carryBits = Real.RemoveDigitFromMantissa(&result, radix: thisNum.radix, limit: thisNum.valueLimit, start: Real.NumDigits)
			thisNum.exponent++
		}
		if (Double(carryBits) >= (Double(thisNum.radix) / 2.0)) {
			Real.AddToMantissa(&result, digit: 1, limit: thisNum.valueLimit, start: Real.NumDigits)
		}
		
		// Create a user point, store all the values back in the class and we're done
        Real.AssignValues(&thisNum.digits, source: result, sstart: Real.NumDigits)
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
        var values = [Digit](count: Real.NumDigits * 2, repeatedValue: 0)
        var otherNumValues = values
        var result = values
        var subValues = values
        var thisNum = self
        var otherNum = num
        
        if num.radix != Int(radix) {
            otherNum = num.convertToRadix(radix)
        }
        
        // Get the numerical values
        Real.CopyValues(digits, destination: &values, dstart: Real.NumDigits)
        Real.CopyValues(otherNum.digits, destination: &otherNumValues, dstart: Real.NumDigits)
        
        // ignore invalid numbers
        if !otherNum.isValid || !thisNum.isValid {
			thisNum.valid = thisNum.isValid ? otherNum.valid : thisNum.valid
            return thisNum
        }
        
        
        let compare = self.compareWith(num)
        if compare == .OrderedAscending {
            // return unchanged if num is less than the modulor
            return self
        }
        
        // Apply the user's decimal point
        thisNum.exponent -= Int(thisNum.userPoint)
        thisNum.userPoint = 0
        otherNum.exponent -= Int(otherNum.userPoint)
        otherNum.userPoint = 0
 
        // Two negatives make a positive
        if otherNum.negative { thisNum.negative = thisNum.isNegative ? false : true }
        
        // Normalise this num
        // This involves multiplying through by the radix until the number runs up against the
        // left edge or MSD (most significant digit)
        if Real.ArrayIsNonZero(values) {
            while(values[Real.NumDigits * 2 - 1] < (thisNum.valueLimit / Digit(thisNum.radix))) {
                Real.AppendDigitToMantissa(&values, digit: 0, radix: thisNum.radix, limit: thisNum.valueLimit, scale:2)
                thisNum.exponent--
            }
        } else {
            Real.AssignValues(&thisNum.digits, source: values, sstart: Real.NumDigits)
            thisNum.exponent = 0
            thisNum.userPoint = 0
            thisNum.negative = false
            return thisNum
        }
        
        // Normalise otherNum so that it cannot be greater than this num
        // This involves multiplying through by the radix until the number runs up
        // against the left edge or MSD (most significant digit)
        // If the last multiply will make otherNum greater than this num, then we
        // don't do it. This ensures that the first division column will always be non-zero.
        if Real.ArrayIsNonZero(otherNumValues) {
            while (otherNumValues[Real.NumDigits * 2 - 1] < (otherNum.valueLimit / Digit(otherNum.radix))) &&
                (otherNumValues[Real.NumDigits * 2 - 1] < (values[Real.NumDigits * 2 - 1] / Digit(otherNum.radix)))
            {
                Real.AppendDigitToMantissa(&otherNumValues, digit: 0, radix: thisNum.radix, limit: thisNum.valueLimit, scale:2)
                otherNum.exponent--
            }
        } else {
            thisNum.valid = .PositiveInfinity
            return thisNum
        }
        
        // Subtract the exponents
        var divisionExponent = thisNum.exponent - otherNum.exponent
        
        // Account for the de-normalising effect of division
        divisionExponent -= (Real.NumDigits - 1) * Int(thisNum.valuePrecision)
        
        // Set the re-normalised values so that we can subtract from self later
        var nself = thisNum
        Real.AssignValues(&nself.digits, source:values, sstart:Real.NumDigits)
     
        // Begin the division
        // What we are doing here is lining the divisor up under the divisee and subtracting the largest multiple
        // of the divisor that we can from the divisee with resulting in a negative number. Basically it is what
        // you do without really thinking about it when doing long division by hand.
        var carryBits : Digit = 0
        for i in (Real.NumDigits-1..<Real.NumDigits * 2).reverse()  {
            // If the divisor is greater or equal to the divisee, leave this result column unchanged.
            if otherNumValues[Real.NumDigits * 2 - 1] > values[i] {
                if i > 0 {
                    values[i - 1] += values[i] * thisNum.valueLimit
                }
                continue
            }
            
            // Determine the quotient of this position (the multiple of  the divisor to use)
            var quotient = values[i] / otherNumValues[Real.NumDigits * 2 - 1];
            carryBits = 0
            for j in 0...i {
                subValues[j] = otherNumValues[j + (Real.NumDigits * 2 - 1 - i)] * quotient + carryBits
                carryBits = subValues[j] / thisNum.valueLimit
                subValues[j] %= thisNum.valueLimit
            }
            subValues[i] += carryBits * thisNum.valueLimit;
            
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
                    subValues[j] = otherNumValues[j + (Real.NumDigits * 2 - 1 - i)] * quotient + carryBits;
                    carryBits = subValues[j] / thisNum.valueLimit;
                    subValues[j] %= thisNum.valueLimit;
                }
                subValues[i] += carryBits * thisNum.valueLimit;
                
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
            for j in 0..<Real.NumDigits * 2 {
                if (subValues[j] > values[j]) {
                    // Since we know that this num is greater than the sub num, then we know
                    // that this will never exceed the bounds of the array
                    var peek = 1
                    while (values[j + peek] == 0) {
                        values[j + peek] = thisNum.valueLimit - 1
                        peek++
                    }
                    values[j+peek]--
                    values[j] += thisNum.valueLimit
                }
                values[j] -= subValues[j]
            }
            
            // Attach the remainder to the next column on the right so that it will be part of the next
            // column's operation
            values[i - 1] += values[i] * thisNum.valueLimit
            
            // Clear the remainder from this column
            values[i] = 0
            subValues[i] = 0
        }
        
        // Remove the fractional part of the division result
        // We know that there must be a non-fractional part since the modulor was tested to
        // be less or equal to the modulee
        while divisionExponent < 0 {
            carryBits = Real.RemoveDigitFromMantissa(&result, radix: thisNum.radix, limit: thisNum.valueLimit, start: Real.NumDigits)
            result[Real.NumDigits - 1] += carryBits * thisNum.valueLimit;
            carryBits = Real.RemoveDigitFromMantissa(&result, radix: thisNum.radix, limit: thisNum.valueLimit)
            divisionExponent++
        }
        
        // Now create a number that is this dividend times the modulor and subtract it from the
        // modulee to obtain the result
        var subNum = zero
        subNum.setElements(thisNum.radix, negative:thisNum.isNegative, exp:divisionExponent, valid:true, userPoint:0)
        subNum.digits = Array(result[Real.NumDigits..<result.count])
        subNum = subNum.multiplyBy(num)
        nself = nself.subtract(subNum)
        
        // Remove any trailing zeros in the decimal places by dividing by the radix until they go away
        thisNum = nself
        Real.CopyValues(nself.digits, destination: &values)
        while (thisNum.exponent < 0) && (values[0] % Digit(thisNum.radix) == 0) {
            carryBits = Real.RemoveDigitFromMantissa(&values, radix: thisNum.radix, limit: thisNum.valueLimit)
            nself.exponent++
        }
        
        // Create a user point, store all the values back in the class and we're done
        nself = thisNum
        Real.AssignValues(&nself.digits, source: values)
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
        
        if num.radix != Int(radix) {
            otherNum.convertToRadix(radix)
        }
        
        // ignore invalid numbers
        if (otherNum.isValid == false || isValid == false) {
            return .OrderedAscending
        }
        
        // Handle differences in sign
        if otherNum.negative != isNegative {
            if otherNum.isNegative { return .OrderedDescending }
            else { return .OrderedAscending }
        }
        
        Real.NormaliseNumbers(&values, otherNum: &otherNum)
        
        let ownLength = Real.NumDigitsInArray(values.digits, radix: radix, precision: valuePrecision)
        let otherLength = Real.NumDigitsInArray(otherNum.digits, radix: radix, precision: valuePrecision)
        let maxLength = max(ownLength, otherLength)
        
        //
        // For a full length number, never compare the last digit because it's
        // subject to rounding problems.
        //
        if maxLength == Int(valuePrecision) * Real.NumDigits {
            values.digits[0] /= Digit(radix)
            values.digits[0] *= Digit(radix)
            otherNum.digits[0] /= Digit(radix)
            otherNum.digits[0] *= Digit(radix)
        }
        
        // Now that we're normalised, do the actual comparison
        compare = .OrderedSame
        for i in (0..<Real.NumDigits).reverse() {
            if (values.digits[i] > otherNum.digits[i] && !isNegative) || (values.digits[i] < otherNum.digits[i] && isNegative) {
                compare = .OrderedDescending
                break
            } else if (values.digits[i] < otherNum.digits[i] && !isNegative) || (values.digits[i] > otherNum.digits[i] && isNegative) {
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
    public func pow(num: Real) -> Real {
        var numCopy = num
        var result = self
        
        if !isValid { return self }
        
        if !num.isValid {
            result.valid = num.valid
            return result
        }
        
        if self.isZero {
            // Zero raised to anything except zero is zero (provided exponent is valid)
            numCopy.valid = num.valid
            if num.isZero { return one }
            return self
        }
        
        let exp = Int(num.doubleValue)
        if num.fractionalPart.isZero && Swift.abs(exp) < 0x8000 {
            return self.raiseToIntPower(exp)
        }
        
        if isNegative {
            result.negative = false
        }
        
        result = (result.log() * num).exp()
        if isNegative {
            if numCopy.isNegative {
                numCopy = -numCopy
            }
            numCopy %= two
            if numCopy == one {
                result.negative = true
            } else if !numCopy.isZero {
                result.valid = .QuietNaN
            }
            
        }
        return result
    }
    
    //
    // Returns the value e^x where x is the value of the receiver.
    //
    public func exp() -> Real {
        var squares = 0
        var result = self
        
        if !isValid { return result }
        
        // Pre-scale the number to aid convergeance
        if isNegative {
            result.negative = false
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
        
        if isNegative {
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
        
        if !isValid || self.isZero { return self }
        
        // oddly-numbered roots of negative numbers should work
        if self.isNegative && (n & 1) == 0 {
            result.valid = .QuietNaN
            return result
        }
        result.negative = false     // we'll fix this later
        
        let original = result
        let root = Real(n, radix: radix)
        
        // Count the number of digits left of the point
        var numDigits = Real.NumDigits * Int(valuePrecision) + Int(exponent) - Int(userPoint)
        var digitNotFound = true
        for i in (0..<Real.NumDigits).reverse() where digitNotFound {
            for j in (0..<valuePrecision).reverse() where digitNotFound {
                if ((digits[i] / Real.pow(radix, Int(j)) % Digit(radix)) == 0) {
                    numDigits--
                } else {
                    digitNotFound = false
                }
            }
        }
        
        // The first guess will be the scaled exponent of this number
        result.exponent -= numDigits / n
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
        let twoEpsilon = two * epsilon
		if (result - one).abs < twoEpsilon {
			result = one
		}
		
        // fix the sign
        result.negative = isNegative
        return result
    }

    
    //
    // Returns the natural logarithm of the receiver.
    //
    public func log() -> Real {
        if !isValid { return self }
        
        var prevIteration = zero
        var i = two
        let eighth = Real(0.125, radix: radix)
        var result = self
        var inverse = false
        var outputFactor : UInt64 = 1
        
        // ln(x) for x <= 0 is inValid
        if self <= prevIteration {
            result.valid = .QuietNaN
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
        let factorNum = Real(Int(outputFactor), radix: radix)
        return result * factorNum
    }
    
    //
    // Takes the "base" log of the receiver.
    //
    public func logOfBase(base: Real) -> Real {
        if !isValid { return self }
        if !base.isValid { return base }
        return self.log()/base.log()
    }
    
    //
    // Takes the base 10 log of the receiver.
    //
    public func log10() -> Real { return logOfBase(Real(10, radix:radix)) }
    
    //
    // Returns the sine of the receiver where the receiver
    // is interpreted as having *mode* angular units.
    //
    public func sinWithTrigMode(mode: TrigMode) -> Real {
        if !isValid { return self }
		
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
            let twoN = Real(i * 2, radix: radix)
            let twoNPlusOne = Real(i * 2 + 1, radix: radix)
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
        let value = result.abs
        if value > one { result /= value }
        
        // Normalise to remove built up error (makes a zero output possible)
        nextTerm = Real(10000, radix:radix)
        Real.NormaliseNumbers(&result, otherNum: &nextTerm)
        result.createUserPoint()
        return result
    }
    
    //
    // Returns the cosine of the receiver where the receiver
    // is interpreted as having *mode* angular units.
    //
    func cosWithTrigMode(mode: TrigMode) -> Real {
        if !isValid { return self }
        
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
            let twoN = Real(i * 2, radix:radix)
            let twoNMinusOne = Real(i * 2 - 1, radix:radix)
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
        let value = result.abs
        if value > one { result /= value }
        
        // Normalise to remove built up error (makes a zero output possible)
        nextTerm = Real(10000, radix:radix)
        Real.NormaliseNumbers(&result, otherNum: &nextTerm)
        result.createUserPoint()
        return result
    }
    
    //
    // Returns the tangent of the receiver where the receiver
    // is interpreted as having *mode* angular units.
    //
    public func tanWithTrigMode(mode: TrigMode) -> Real {
        if !isValid { return self }
        
        let original = self.toRadiansFrom(mode)
        var result = original.sinWithTrigMode(mode) / original.cosWithTrigMode(mode)
        
        // Normalise to remove built up error (makes a zero output possible)
        var nextTerm = Real(10000, radix:radix)
        Real.NormaliseNumbers(&result, otherNum: &nextTerm)
        result.createUserPoint()
        return result
    }

    public func asin(mode: TrigMode) -> Real {
        if !isValid { return self }
        
        let half = Real(0.5, radix:radix)
        let minusHalf = Real(-0.5, radix:radix)
        
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
            let twoN = Real(i * 2, radix:radix)
            let twoNMinusOne = Real(i * 2 - 1, radix:radix)
            factorial *= twoNMinusOne
            factorial /= twoN
            
            nextTerm = powerCopy
            let twoNPlusOne = Real(i * 2 + 1, radix:radix)
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
    
    public func acos(mode: TrigMode) -> Real {
        // arccos = π/2 - arcsin
        if !isValid { return self }
        var original = asin(.radians)
        original = π / two - original
        return original.radiansToMode(mode)
    }
    
    public func atan(mode: TrigMode) -> Real {
        if !isValid { return self }
        let minusOne = Real(-1, radix: radix)
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
            result = (π / two) / two
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
                
                powerCopy *= original * original
                factorial += two
                nextTerm = factorial
                nextTerm *= powerCopy
                nextTerm = nextTerm.inverse
                if nextTerm.isValid { result += nextTerm }
                
                powerCopy *= original * original
                factorial += two
                nextTerm = factorial
                nextTerm *= powerCopy
                nextTerm = nextTerm.inverse
                if nextTerm.isValid { result -= nextTerm }
            }
        }
        
        return result.radiansToMode(mode)
    }

    public func sinh() -> Real {
        if !isValid { return self }
        var result = self
        var original = exp()
        result.appendDigit("-", useComplement:0)
        result = result.exp()
        original -= result
        original /= two
        return original
    }

    public func cosh() -> Real {
        if !isValid { return self }
        var original = exp()
		var result = self
        result.negative = !isNegative ? true : false
        original += result.exp()
        original /= two
        return original
    }

    public func tanh() -> Real {
        if !isValid { return self }
        return self.sinh() / self.cosh()
    }
    
    public func asinh() -> Real {
        if !isValid { return self }
        var original = self
        let result = (self * self + one).sqrt()
        original += result
        return original.log()
    }
    
    public func acosh() -> Real {
        if !isValid { return self }
        var original = self
		let result = (self * self - one).sqrt()
        original += result
		return original.log()
    }
    
    public func atanh() -> Real {
        if !isValid { return self }
        var result = (one + self) / (one - self)
        result = result.log()
        return result / two
    }
    
    public func atan2(y:Real) -> Real {
        let x = self
        let k3 = Real(3, radix: radix)
        let k4 = four
        
        // Algorithm shamelessly stolen from qd_real
        if x.isZero {
            if y.isZero {
                // Actually an error
                var x = zero
                x.valid = .QuietNaN
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
        return (y/x).atan(.radians)
    }
    
    public func hypot(y:Real) -> Real {
        var x = self
        x = x*x + y*y
        return x.sqrt()
    }
    
    //
    // Calculates a factorial in the most basic way.
    //
    public func factorial() -> Real {
        if !isValid { return self }
        
        var result = self
        
        // Negative numbers have no factorial equivalent
        if isNegative || !fractionalPart.isZero {
            result.valid = .QuietNaN
            return result
        }
        
        // Factorial zero is 1
        if isZero {
            return one
        } else {
            // Copy this num and start subtracting down to one
            var counter = self - one
            
            // Perform the basic factorial
            while counter > zero && result.isValid {
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
        if !isValid { return self }
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
        if !isValid { return self }
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
		if !isValid { return self }
        if isZero {
            var result = self
            result.valid = .PositiveInfinity
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
		// b is a dummy (sorry b) just so the binary op algorithm can be applied here as well
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
        if !isValid { return self}
        return self.moduloBy(one)
    }
    
    //
    // Sets the receiver to (receiver - (receiver modulo 1))
    //
    public var wholePart: Real {
        if !isValid { return self }
		
        let numb = self.abs
        
        var whole = numb.subtract(numb.fractionalPart)
        whole.negative = isNegative
        return whole
    }
    
    //
    // Returns the approximate value of the receiver as a double
    //
    public var doubleValue: Double {
        // Return NaN if number is not valid
        if !isValid {
            return Double.NaN
        }
        
        // Extract all the digits out and put them in the double
        var retVal = 0.0
        for i in (0..<Real.NumDigits).reverse() {
            let currentValue = Int64(digits[i])
            for j in (0..<valuePrecision).reverse() {
                let power : Digit = Real.pow(radix, Int(j))
                let digit = currentValue / Int64(power) % Int64(radix)
                retVal = (retVal * Double(radix)) + Double(digit)
            }
        }
        
        // Apply the sign
        if isNegative {
            retVal *= -1.0
        }
        
        // Apply the exponent
        retVal *= Real.pow(radix, exponent - Int(userPoint))
        return retVal
    }
	
	//
	// Interprets the given int as an exponent and formats it as a string. Why did I do this?
	//
	private func exponentStringFromInt(exp: Int) -> String {
        var workingExponent = exp;
        var exponentIsNegative = false
        var digits = [Character](count: Real.MaxExponentLength, repeatedValue: "0")
        var currentPosition = Real.MaxExponentLength - 1  // index of the end of the string
        var lastNonZero = currentPosition
        
        if workingExponent == 0 { return "" }
        
        // Check for a negative exponent
        if workingExponent < 0 {
            workingExponent = -workingExponent
            exponentIsNegative = true
        }
        
        // Work right to left and fill in the digits
        while currentPosition > Real.MaxExponentLength - Int(valuePrecision) - 2 {
            digits[currentPosition] = Real.ValidDigits[workingExponent % radix]
            
            // Keep checking for the leftmost non-zero digit
            if digits[currentPosition] != "0" {
                lastNonZero = currentPosition
            }
            
            workingExponent /= radix
            currentPosition--
        }
        
        // If all the digits were zeros, force the display of at least one zero
        if lastNonZero == Real.MaxExponentLength {
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
        if !isValid {
            // Return an empty string instead of nil because its a little safer
            return ""
        }
        return exponentStringFromInt(exponent)
    }
    
    //
    // Returns the mantissa of the receiver as a string.
    //
    public var mantissaString: String {
        var mantissa = ""
        var exponent = ""
        
        limitedString(Real.NumDigits * Int(valuePrecision), fixedPlaces:0, fillLimit:false, complement:0, mantissa:&mantissa, exponent:&exponent)
        
        return mantissa
    }
    
    //
    // Returns an approximate string representation of the receiver
    //
    public var description: String {
        // Get the mantissa string
        let mantissa = mantissaString
        
        // Append the exponent string
        if exponent != 0 {
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
    private func limitedString(var lengthLimit: Int, var fixedPlaces places:Int, fillLimit fill:Bool, complement:Int, inout mantissa mantissaOut:String, inout exponent exponentOut:String) {
        var digitStr = [Character](count: Real.MaxMantissaLength, repeatedValue: "0")
        var values = [Digit](count: Real.NumDigits, repeatedValue: 0)
        var zeros = 0
        let point = NSLocale.currentLocale().objectForKey(NSLocaleDecimalSeparator) as! String
        
        // Handle the "not-a-number" case
        if !isValid {
            mantissaOut = "NaN"
            exponentOut = ""
            return
        }
        
		// Limit the length of the output string
		if lengthLimit > Real.NumDigits * Int(valuePrecision) {
			lengthLimit = Real.NumDigits * Int(valuePrecision)
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
        Real.CopyValues(digits, destination: &values)
        var exponentCopy = exponent;
		var userPointCopy = Int(userPoint)
        
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
                
                digitStr[0] = "0"
                while d < point.count() {
                    digitStr[1 + d] = point[d]
                    d++
                }
                for i in 1+d..<places+2 {
                    digitStr[i] = "0"
                }
                
				mantissaOut = String(digits[0..<places+2])
                exponentOut = ""
                return
            }
            
            // Too many digits so strip them back
			var carryBits : Digit = 0
            while exponentCopy < 0 {
                carryBits = Real.RemoveDigitFromMantissa(&values, radix: radix, limit: valueLimit)
                exponentCopy++
                digitsInNumber--
            }
            
            // Apply round to nearest
            if Double(carryBits) >= (Double(radix) / 2.0) {
                Real.AddToMantissa(&values, digit: 1, limit: valueLimit)
                
                // In the incredibly unlikely case that this rounding increases the number of digits
                // in the number past the precision, then bail out.
                if (values[Real.NumDigits - 1] / valueLimit != 0) {
					mantissaOut = "∞"
                    exponentOut = ""
                    return
                }
            }
            
            // Not enough digits so pad them out
            while (exponentCopy > 0) {
                Real.AppendDigitToMantissa(&values, digit: 0, radix: radix, limit: valueLimit)
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
				carryBits = Real.RemoveDigitFromMantissa(&values, radix: radix, limit: valueLimit)
				
				digitsInNumber--
				if userPointCopy > 0 {
					userPointCopy--
				} else {
					exponentCopy++
				}
			}
			
            // Apply round to nearest
			if Double(carryBits) >= (Double(radix) / 2.0) {
				Real.AddToMantissa(&values, digit: 1, limit: valueLimit)
				
				// If by shear fluke that cause the top digit to overflow, then shift back by one digit
				if (values[Real.NumDigits - 1] / valueLimit != 0) {
					Real.RemoveDigitFromMantissa(&values, radix: radix, limit: valueLimit)
					
					if (userPointCopy > 0) {
						userPointCopy--;
					} else {
						exponentCopy++
					}
				}
				
                // We may have changed the number of digits... recount
                digitsInNumber = Int(Real.NumDigitsInArray(values, radix: radix, precision: valuePrecision))
            }
        }
        
        // Scientific notation weirdisms
        if fill && places == 0 {
            let diff = (digitsInNumber - 1) - userPointCopy
            userPointCopy += diff
            exponentCopy += diff
            
            // Not enough digits so pad them out
            while (digitsInNumber < lengthLimit) {
                Real.AppendDigitToMantissa(&values, digit: 0, radix: radix, limit: valueLimit)
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
            
			complementNumber = Real(mantissa: complementBits, exponent: 0, isNegative: false, radix: radix, userPointAt: 0)
            mantissaNumber = complementNumber
            Real.CopyValues(mantissaNumber.digits, destination: &values)
            
			var carryBits : Digit = 0
			while (
				(mantissaNumber.compareWith(complementNumber) == .OrderedDescending  ||
					(mantissaNumber.compareWith(complementNumber) == .OrderedSame  && !isNegative)) && !mantissaNumber.isZero
				)
			{
				carryBits = Real.RemoveDigitFromMantissa(&mantissaNumber.digits, radix: radix, limit: valueLimit)
				
				if (userPointCopy > 0) {
					userPointCopy--;
				} else {
					exponentCopy++
				}
			}
            // Apply round to nearest
            if (Double(carryBits) >= (Double(radix) / 2.0)) {
                Real.AddToMantissa(&mantissaNumber.digits, digit: 1, limit: valueLimit)
                
                // If by shear fluke that cause the top digit to overflow, then shift back by one digit
                if (values[Real.NumDigits - 1] / valueLimit != 0) {
                    Real.RemoveDigitFromMantissa(&mantissaNumber.digits, radix: radix, limit: valueLimit)
                    
					if (userPointCopy > 0) {
						userPointCopy--;
					} else {
						exponentCopy++
					}
                }
            }
            if (mantissaNumber.compareWith(complementNumber) == .OrderedDescending ||
               (mantissaNumber.compareWith(complementNumber) == .OrderedSame && !isNegative))
            {
				mantissaOut = "∞"
                exponentOut = ""
                return
            }
            
            if isNegative {
                complementNumber = complementNumber.multiplyBy(two)
                complementNumber = complementNumber.subtract(mantissaNumber)
                Real.CopyValues(complementNumber.digits, destination: &values)
                digitsInNumber = complementNumber.mantissaLength
            } else {
                Real.CopyValues(mantissaNumber.digits, destination: &values)
                digitsInNumber = mantissaNumber.mantissaLength
            }
            
        } else if isNegative {
            digitStr[currentChar] = "-"
            currentChar++
        }
        
        // Write any leading zeros to the string
        if userPointCopy >= digitsInNumber {
            digitStr[currentChar] = "0"
            currentChar++
            
            if (userPointCopy - digitsInNumber > 0) {
                var d = 0
                
                while d < point.count() {
                    digitStr[currentChar++] = point[d]
                    d++
                }
            }
            
            for _ in 0..<userPointCopy - digitsInNumber {
                digitStr[currentChar] = "0"
                currentChar++
            }
        }
        
        // Write the digits out to the string
        digitsInNumber--
        while digitsInNumber >= 0 {
            let power: Double = Real.pow(radix, digitsInNumber % Int(valuePrecision))
            let nextDigit = Real.ValidDigits[(Int(Double(values[digitsInNumber / Int(valuePrecision)]) / power) % radix)]
            
            if userPointCopy <= digitsInNumber {
                if userPointCopy != 0 && userPointCopy == (digitsInNumber + 1) {
                    var d = 0
                    while d < point.count() {
                        digitStr[currentChar++] = point[d]
                        d++
                    }
                }
                
                digitStr[currentChar] = nextDigit
                currentChar++
            } else if (nextDigit == "0" && !fill && complement == 0 && userPointCopy > digitsInNumber) {
                zeros++
            } else {
                if (userPointCopy != 0 && userPointCopy == (digitsInNumber + 1 + zeros)) {
                    var d = 0
					while d < point.count() {
						digitStr[currentChar++] = point[d]
						d++
					}
                }
				
                for _ in 0..<zeros {
                    digitStr[currentChar] = "0"
                    currentChar++
                }
                digitStr[currentChar] = nextDigit
                currentChar++
                zeros = 0
            }
            
            digitsInNumber--
        }
        
        mantissaOut = String(digitStr[0..<currentChar])
        exponentOut = self.exponentStringFromInt(exponentCopy)
    }
    
    
    static func Test () {
        
        var pass = 0
        var test = 1
        
        func testCase (n1: Real, n2: Real? = nil, op: ((Real,Real)->Real)? = nil, opstr: String = "", result: String) {
            let lpass: String
            if pass != 0 { return }
            if let op = op, n2 = n2 {
                let answer = "\(op(n1,n2))"
                lpass = result == answer ? "Pass" : "FAIL!"
                print("T\(test):a \(opstr) b \t= \(answer) -> \(lpass)")
            } else {
                let answer = "\(n1)"
                lpass = result == answer ? "Pass" : "FAIL!"
                print("T\(test):\(opstr) \t= \(answer) -> \(lpass)")
            }
            if lpass == "FAIL!" { pass = test }
            test++
        }
        
        // Basic tests of the functionality
        let one = Real(1)
        let two = Real(2)
        let c32 = Real(256)
        let a = Real(123456.78e10)
        let b = Real("-1234567890.12345678901234567890e1000")
        testCase(a, opstr:"a", result: "12345678e8")
        testCase(b, opstr:"b", result: "-1234567890.1234567890123456789e1000")
        testCase(a, n2: b, op: +, opstr: "+", result: "-1234567890123456789012345678900000000000000000000000000000000000e946")
        testCase(a, n2: b, op: -, opstr: "-", result: "1234567890123456789012345678900000000000000000000000000000000000e946")
        testCase(a, n2: b, op: *, opstr: "*", result: "-1524157764060357776406035777639079420e988")
        testCase(a, n2: b, op: /, opstr: "/", result: "-9.999999269999993438999940294909456682855055814063007907973381963e-995")
        testCase(a, n2: b, op: %, opstr: "%", result: "-1234567800000000")
        
        testCase(one.exp(),              opstr:"exp(1)",   result: "2.718281828459045235360287471352662497757247093699959574966967633")
        testCase(two.sqrt(),             opstr:"sqrt(2)",  result: "1.414213562373095048801688724209698078569671875376948073176679738")
        testCase(one.exp().log(),        opstr:"ln(e)",    result: "1.000000000000000000000000000000000000000000000000000000000000001")
        testCase(two.pow(c32),           opstr:"2**256",   result: "1157920892373161954235709850086879078532699846656405640394575840e14")
        testCase(two.pow(c32+Real(0.5)), opstr:"2**256.5", result: "1637547430149282552351024030859592566150148649424864741236437244e14")
        testCase(c32.factorial(),        opstr:"256!",     result: "8578177753428426541190822716812326251577815202794856198596556556e443")
        testCase(one.pi,                 opstr:"π",        result: "3.141592653589793238462643383279502884197169399375105820974944666")
        
        var x = one.sinWithTrigMode(.radians)
        testCase(x,                opstr:"sin(1)",           result: "0.84147098480789650665250232163029899962256306079837106567275")
        testCase(x.asin(.radians), opstr:"arcsin(sin(1))",   result: "0.999999999999999999999999999999999999999999999999999999999996872")
        x = one.cosWithTrigMode(.radians)
        testCase(x,                opstr:"cos(1)",           result: "0.5403023058681397174009366074429766037323104206179222276701")
        testCase(x.acos(.radians), opstr:"arccos(cos(1))",   result: "0.999999999999999999999999999999999999999999999999999999999996737")
        x = one.tanWithTrigMode(.radians)
        testCase(x,                opstr:"tan(1)",           result: "1.55740772465490223050697480745836017308725077238152003838394")
        testCase(x.atan(.radians), opstr:"arctan(tan(1))",   result: "0.999999999999999999999999999999999999999999999999999999999998115")
        x = one.sinh()
        testCase(x,                opstr:"sinh(1)",          result: "1.175201193643801456882381850595600815155717981334095870229565416")
        testCase(x.asinh(),        opstr:"arcsinh(sinh(1))", result: "1.000000000000000000000000000000000000000000000000000000000000001")
        x = one.cosh()
        testCase(x,                opstr:"cosh(1)",          result: "1.543080634815243778477905620757061682601529112365863704737402217")
        testCase(x.acosh(),        opstr:"arccosh(cosh(1))", result: "1.000000000000000000000000000000000000000000000000000000000000001")
        x = one.tanh()
        testCase(x,                opstr:"tanh(1)",          result: "0.761594155955764888119458282604793590412768597257936551596810501")
        testCase(x.atanh(),        opstr:"arctanh(tanh(1))", result: "0.999999999999999999999999999999999999999999999999999999999999989")
        
        let n = Real("FFFF0FFFFF0F", radix: 16)
        let m = Real("FAAAAFF12F0F", radix: 16)
        testCase(n,                       opstr:"a",  result: "FFFF0FFFFF0F")
        testCase(m,                       opstr:"b",  result: "FAAAAFF12F0F")
        testCase(n.notUsingComplement(0), opstr:"~a", result: "F00000F0")
        testCase(n, n2: m, op: &,         opstr: "&", result: "FAAA0FF12F0F")
        testCase(n, n2: m, op: |,         opstr: "|", result: "FFFFAFFFFF0F")
        testCase(n, n2: m, op: ^,         opstr: "^", result: "555A00ED000")
        
        if pass == 0 { print("Success! All tests passed!") }
        else { print("*** Failed test case \(pass)! ***") }
    }
    
    //
    // Note: These are all base 10 numbers
    //
    static let PI = ONE.pi
    static let π = PI
    static let E = ONE.exp()
    static let e = E
    static let LN2 = TWO.log()
    static let LOG2E = ONE / LN2
    static let LN10 = TEN.log()
    static let LOG10E = E.log10()
    static let SQRT2 = TWO.sqrt()
    static let SQRT1_2 = ONE/SQRT2
    static let ZERO = Real(0)
    static let ONE = Real(1)
    static let TWO = Real(2)
    static let TEN = Real(10)
    static let epsilon : Real = ONE.epsilon
	
}

