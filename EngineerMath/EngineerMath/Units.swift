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
	
	struct UnitType {
		var upper = UnitBag()	// units to positive power
		var lower = UnitBag()	// units to negative power
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
		// Basic conversion constants
		let inchPerFoot  = 12.0
		let cmPerInch    = 2.54
		let feetPerMile  = 5280.0
		let secsPerMin   = 60.0
		let minsPerHour  = 60.0
		let secsPerHour  = secsPerMin * minsPerHour
		let degFOffset   = 32.0
		let cmPerMeter   = 100.0
		let mmPerMeter   = 1000.0
		let degFPerdegC  = 9.0/5.0
		let absoluteZero = 273.16
		
		// Define some baseline units where SI units are the baseline
		// By convention the base units come first
		defineUnit(.Length,		 name: "meter",		 abbreviation: "m")
		defineUnit(.Time,		 name: "second",	 abbreviation: "s")
		defineUnit(.Temperature, name: "kelvin",	 abbreviation: "K")
		defineUnit(.Mass,		 name: "kilogram",	 abbreviation: "kg")
		defineUnit(.Current,     name: "ampere",	 abbreviation: "A")
		defineUnit(.Luminance,	 name: "candela",	 abbreviation: "cd")
		defineUnit(.Amount,		 name: "mole",		 abbreviation: "mol")
		
		// Add other derived units
		defineUnit(.Length,		 name: "centimeter", abbreviation: "cm", toBase: { $0/cmPerMeter } )
		defineUnit(.Length,		 name: "millimeter", abbreviation: "mm", toBase: { $0/mmPerMeter } )
		defineUnit(.Length,		 name: "foot",		 abbreviation: "ft", toBase: { inchPerFoot*cmPerInch*$0/cmPerMeter } )
		defineUnit(.Length,		 name: "mile",		 abbreviation: "mi", toBase: { feetPerMile*inchPerFoot*cmPerInch*$0/cmPerMeter } )
		defineUnit(.Time,		 name: "hour",		 abbreviation: "hr", toBase: { $0*secsPerHour } )
		defineUnit(.Time,		 name: "minute",	 abbreviation: "min",toBase: { $0*secsPerMin } )
		defineUnit(.Temperature, name: "kahrenheit", abbreviation: "°F", toBase: { ($0-degFOffset)/degFPerdegC }, fromBase: { degFPerdegC*$0+degFOffset } )
		defineUnit(.Temperature, name: "celsius",	 abbreviation: "°C", toBase: { $0-absoluteZero}, fromBase: { $0+absoluteZero } )
	}
	
	//
	// Initialize with unit abbreviation strings like Units("m / s s") for m/s²
	//
	init (_ unitString : String) {
		units = UnitType()
		
		// define some standard units
		if Units.abbreviations.count == 0 { Units.defineUnits() }
		
		// break apart the unit string to actual defined units
		let unitArrays = unitString.componentsSeparatedByString("/")
		
		// extract the lower units
		if let lowerArrays = unitArrays.last?.componentsSeparatedByString(" ") where unitArrays.count > 1 {
			for unit in lowerArrays {
				let abbreviation = unit.stringByReplacingOccurrencesOfString(" ", withString: "")
				if abbreviationIsDefined(abbreviation) { self.units.upper.add(abbreviation) }
			}
		}
		
		// extract the upper units
		if let upperArrays = unitArrays.first?.componentsSeparatedByString(" ") {
			for unit in upperArrays {
				let abbreviation = unit.stringByReplacingOccurrencesOfString(" ", withString: "")
				if abbreviationIsDefined(abbreviation) { self.units.lower.add(abbreviation) }
			}
		}
	}
	
	//
	// Initialize with the base unit for the UnitCategory
	//
	init (_ unit : UnitCategory) {
		units = UnitType()
		if let abbreviation = Units.abbreviations[unit]?.first {
			units.upper.add(abbreviation)
		}
	}
	
	init (_ units : UnitType) {
		self.units = units
	}
	
	var baseUnit : Units {
		var base = UnitType()
		
		for unit in units.upper {
			let baseAbbreviation = baseUnit(unit.0)
			if !baseAbbreviation.isEmpty { base.upper.add(baseAbbreviation) }
		}
		for unit in units.lower {
			let baseAbbreviation = baseUnit(unit.0)
			if !baseAbbreviation.isEmpty { base.lower.add(baseAbbreviation) }
		}
		return Units(base)
	}
	
	func baseUnit (abbreviation: String) -> String {
		for baseUnitAbbreviations in Units.abbreviations {
			for unitAbbreviation in baseUnitAbbreviations.1 {
				if unitAbbreviation == abbreviation {
					let unit = baseUnitAbbreviations.0
					if let baseAbbreviation = Units.abbreviations[unit]?.first {
						return baseAbbreviation
					}
				}
			}
		}
		return ""
	}
	
	func abbreviationIsDefined (abbreviation: String) -> Bool {
		for baseUnitAbbreviations in Units.abbreviations {
			for unitAbbreviation in baseUnitAbbreviations.1 {
				if unitAbbreviation == abbreviation { return true }
			}
		}
		return false
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
	
//	private static func convert(number: Double, fromType: UnitType, toType: UnitType) -> Double? {
//		if fromType != toType { return nil }  // illegal conversion
////		if let convertToBase = toBase[fromType.baseUnit]?[fromType.activeUnit] {
////			var baseNumber = convert(number, power: fromType.order, conversion: convertToBase)
////			if let convertToType = fromBase[toType.baseUnit]?[toType.activeUnit] {
////				return convert(baseNumber, power: toType.order, conversion: convertToType)
////			}
////		}
//		return nil
//	}
	
	private func getMatchingUnit (unit: UnitType, inUnits: Units) -> UnitType? {
//		for funit in inUnits.units {
//			if unit.baseUnit == funit.baseUnit { return funit }
//		}
		return nil
	}
	
	static func convert (number: Double, fromType: Units, toType: Units) -> Double? {
		if fromType.isCompatibleWith(toType) {
			var x = number
//			for unit in fromType.units {
//				if let toUnit = fromType.getMatchingUnit(unit, inUnits: toType) {
//					x = Units.convert(x, fromType: unit, toType: toUnit)!
//				}
//			}
			return x
		}
		return nil
	}
	
	func convert (number: Double, toUnit: Units) -> Double? {
		return Units.convert(number, fromType: self, toType: toUnit)
	}
	
	func isCompatibleWith (unit: Units) -> Bool {
		return (self.baseUnit == unit.baseUnit)
	}
	
	func inverse () -> Units {
		// invert units by negating powers
//		var result: Set<UnitType> = []
//		for unit in units {
//			var newUnit = unit
//			newUnit.order = -newUnit.order
//			result.insert(newUnit)
//		}
		return Units("")
	}
	
	func div (x: Units) -> Units {
		return x.inverse().mul(self)
	}
	
	func mul (x: Units) -> Units {
		var result = self.units
//		for unit in x.units {
//			var newUnit = unit
//			if let runit = getMatchingUnit(unit, inUnits: self) {
//				result.remove(runit)
//				newUnit.order += runit.order
//			}
//			result.insert(unit)
//		}
		return Units("")
	}

}

// required for UnitType Hashable protocol
func == (lhs: Units, rhs: Units) -> Bool {
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

