//
//  Units.swift
//  DimensionalTests
//
//  Created by Michael Griebling on 10Apr2015.
//  Copyright (c) 2015 Solinst Canada. All rights reserved.
//

import Foundation

class Units {
	
	enum UnitCategory {
		case Time, Length, Mass, Temperature, Current, Luminance, Amount
	}
	
	typealias UnitBag = Bag<String>
	
	struct UnitType : Equatable {
		var upper: UnitBag		// units to positive power
		var lower: UnitBag		// units to negative power
		var description: String {
			var unitString = ""
			for unit in upper {
				if !unitString.isEmpty { unitString += "∙" }
				unitString += unit.0 + Units.getStringForPower(unit.1)
			}
			for unit in lower {
				if !unitString.isEmpty { unitString += "∙" }
				unitString += unit.0 + Units.getStringForPower(-unit.1)
			}
			return unitString
		}
	}
	
	typealias convertFunction = (Double) -> Double
	
	// contains all the aggregated units e.g., m∙s⁻¹
	private var units: UnitType
	
	private static let emptyArrays : [UnitCategory: [String]] = [.Time: [], .Length: [], .Mass: [], .Temperature: [], .Current: [], .Luminance: [], .Amount: []]
	private static let emptyConversions : [UnitCategory: [convertFunction]] = [.Time: [], .Length: [], .Mass: [], .Temperature: [], .Current: [], .Luminance: [], .Amount: []]
	
	private static var fromBase      : [UnitCategory: [convertFunction]] = emptyConversions
	private static var toBase        : [UnitCategory: [convertFunction]] = emptyConversions
	private static var names		 : [UnitCategory: [String]]	 = emptyArrays
	private static var abbreviations : [UnitCategory: [String]]	 = emptyArrays
	
	private static let powers = "⁰¹²³⁴⁵⁶⁷⁸⁹"
	private static func getStringForPower (power: Int) -> String {
		var result = ""
		var sign = ""
		var rpower = power
		if power < 0 { sign = "⁻"; rpower = abs(power) }
		if rpower <= 1 && power >= 0 { return "" }	// handle case of x^1 = x and x^0 = 1
		while rpower > 0 {
			let digit = rpower % 10; rpower /= 10
			let raisedDigit = powers[advance(Units.powers.startIndex, digit)]
			result = [raisedDigit] + result
		}
		return sign + result
	}
	
	var description: String {
		return units.description
	}
	
	static func defineUnit (unit: UnitCategory, name: String, abbreviation: String, toBase: convertFunction = { $0 }, fromBase: convertFunction = { $0 }) {
		Units.toBase[unit]?.append(toBase)
		Units.fromBase[unit]?.append(fromBase)
		Units.names[unit]?.append(name)
		Units.abbreviations[unit]?.append(abbreviation)
	}
	
	static func defineUnits () {
		defineUnit(.Length,		 name: "meter",		 abbreviation: "m")
		defineUnit(.Length,		 name: "centimeter", abbreviation: "cm", toBase: { $0/100 } )
		defineUnit(.Length,		 name: "millimeter", abbreviation: "mm", toBase: { $0/1000 } )
		defineUnit(.Length,		 name: "foot",		 abbreviation: "ft", toBase: { 12*2.54*$0/100 } )
		defineUnit(.Length,		 name: "mile",		 abbreviation: "mi", toBase: { 5280*12*2.54*$0/100 } )
		defineUnit(.Time,		 name: "hour",		 abbreviation: "hr", toBase: { $0*3600 } )
		defineUnit(.Time,		 name: "second",	 abbreviation: "s")
		defineUnit(.Temperature, name: "Fahrenheit", abbreviation: "°F", toBase: { 5*($0-32)/9 }, fromBase: { 9*$0/5+32 } )
		defineUnit(.Temperature, name: "Celsius",	 abbreviation: "°C")
	}
	
	//
	// Initialize with unit abbreviation strings like Units("m", "s s") for m/s²
	//
	init (_ upper : String, _ lower : String = "") {
		if Units.abbreviations.count == 0 { Units.defineUnits() }  // define some standard units
	}
	
	private static func convert(number: Double, power: Int, conversion: convertFunction) -> Double {
		var lpower: Int = abs(power)
		var baseNumber = number
		while lpower > 0 {
			baseNumber = conversion(baseNumber)
			lpower--
		}
		if power < 0 { baseNumber = 1 / baseNumber }
		return baseNumber
	}
	
	private static func convert(number: Double, fromType: UnitType, toType: UnitType) -> Double? {
		if fromType.baseUnit != toType.baseUnit { return nil }  // illegal conversion
		if let convertToBase = toBase[fromType.baseUnit]?[fromType.activeUnit] {
			var baseNumber = convert(number, power: fromType.order, conversion: convertToBase)
			if let convertToType = fromBase[toType.baseUnit]?[toType.activeUnit] {
				return convert(baseNumber, power: toType.order, conversion: convertToType)
			}
		}
		return nil
	}
	
	private func getMatchingUnit (unit: UnitType, inUnits: Units) -> UnitType? {
		for funit in inUnits.units {
			if unit.baseUnit == funit.baseUnit { return funit }
		}
		return nil
	}
	
	static func convert (number: Double, fromType: Units, toType: Units) -> Double? {
		if fromType.isCompatibleWith(toType) {
			var x = number
			for unit in fromType.units {
				if let toUnit = fromType.getMatchingUnit(unit, inUnits: toType) {
					x = Units.convert(x, fromType: unit, toType: toUnit)!
				}
			}
			return x
		}
		return nil
	}
	
	func convert (number: Double, toUnit: Units) -> Double? {
		return Units.convert(number, fromType: self, toType: toUnit)
	}
	
	func isCompatibleWith (unit: Units) -> Bool {
		return (self.units == unit.units)
	}
	
	func inverse () -> Units {
		// invert units by negating powers
		var result: Set<UnitType> = []
		for unit in units {
			var newUnit = unit
			newUnit.order = -newUnit.order
			result.insert(newUnit)
		}
		return Units(unit: result)
	}
	
	func div (x: Units) -> Units {
		return x.inverse().mul(self)
	}
	
	func mul (x: Units) -> Units {
		var result = self.units
		for unit in x.units {
			var newUnit = unit
			if let runit = getMatchingUnit(unit, inUnits: self) {
				result.remove(runit)
				newUnit.order += runit.order
			}
			result.insert(unit)
		}
		return Units(unit: result)
	}

}

// required for UnitType Hashable protocol
func == (lhs: Units.UnitType, rhs: Units.UnitType) -> Bool {
	return (lhs.baseUnit == rhs.baseUnit)
}

// convenience functions for units
func / (lhs: Units, rhs: Units) -> Units { return lhs.div(rhs) }
func * (lhs: Units, rhs: Units) -> Units { return lhs.mul(rhs) }

//Units.defineUnit(.Length, name: "meter", abbreviation: "m")
//Units.defineUnit(.Length, name: "centimeter", abbreviation: "cm", toBase: { $0/100 } )
//let millimeters = Units(base: .Length,		name: "millimeter", abbreviation: "mm", toBase: { $0/1000 } )
//let feet	    = Units(base: .Length,		name: "foot",		abbreviation: "ft", toBase: { 12*2.54*$0/100 } )
//let miles		= Units(base: .Length,		name: "mile",		abbreviation: "mi", toBase: { 5280*12*2.54*$0/100 } )
//let hours		= Units(base: .Time,		name: "hour",		abbreviation: "hr", toBase: { $0*3600 } )
//let seconds		= Units(base: .Time,		name: "second",		abbreviation: "s")
//let degreesF	= Units(base: .Temperature, name: "fahrenheit",	abbreviation: "°F", toBase: { 5*($0-32)/9 }, fromBase: { 9*$0/5+32 } )
//let degreesC	= Units(base: .Temperature, name: "celsius",	abbreviation: "°C")
//let metersPerS  = meters / seconds
//let milesPerHr  = miles / hours

