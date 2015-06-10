//
//  Units.swift
//  DimensionalTests
//
//  Created by Michael Griebling on 10Apr2015.
//  Copyright (c) 2015 Solinst Canada. All rights reserved.
//

import Foundation

class Units : Printable, Equatable {
	
	enum UnitCategory {
		case Time, Length, Mass, Temperature, Current, Luminance, Amount
	}
	
	typealias UnitBag = Bag<String>
	
	struct UnitType {
		private static let rdot = "∙"
		var upper = UnitBag()	// units to positive power
		var lower = UnitBag()	// units to negative power
		var description: String {
			var unitString = ""
			for unit in upper {
				if !unitString.isEmpty { unitString += UnitType.rdot }
				unitString += unit.0 + Units.getStringForPower(unit.1)
			}
			for unit in lower {
				if !unitString.isEmpty { unitString += UnitType.rdot  }
				unitString += unit.0 + Units.getStringForPower(-unit.1)
			}
			return unitString
		}
		
		var baseUnit : UnitType {
			var base = UnitType()
			for (abbrev, _) in upper {
				let baseAbbreviation = Units.baseUnit(abbrev)
				if !baseAbbreviation.isEmpty { base.upper.add(baseAbbreviation) }
			}
			for (abbrev, _) in upper {
				let baseAbbreviation = Units.baseUnit(abbrev)
				if !baseAbbreviation.isEmpty { base.lower.add(baseAbbreviation) }
			}
			return base
		}
		
		func convertToBaseUnits (x: Double) -> Double {
			let base = baseUnit
			var number = x
			for (abbrev, power) in base.upper {
				switch Units.definitions[abbrev]! {
					case let .NonBaseUnit(_, _, _, convertToBase):
						number = Units.convert(number, power: power, conversion: convertToBase)
					default: break
				}
			}
			for (abbrev, power) in base.lower {
				switch Units.definitions[abbrev]! {
					case let .NonBaseUnit(_, _, _, convertToBase):
						number = Units.convert(number, power: -power, conversion: convertToBase)
					default: break
				}
			}
			return number
		}
		
		func convert (x: Double, toUnits units: UnitType) -> Double {
			var number = x
			for (abbrev, power) in units.upper {
				switch Units.definitions[abbrev]! {
					case let .NonBaseUnit(_, _, convertToUnits, _):
						number = Units.convert(number, power: power, conversion: convertToUnits)
					default: break
				}
			}
			for (abbrev, power) in units.lower {
				switch Units.definitions[abbrev]! {
					case let .NonBaseUnit(_, _, convertToUnits, _):
						number = Units.convert(number, power: -power, conversion: convertToUnits)
					default: break
				}
			}
			return number
		}
	}
	
	typealias convertFunction = (Double) -> Double
	
	// contains all the aggregated units e.g., m∙s⁻¹
	private var units: UnitType
	
	// Basic unit definitions
	enum UnitDefinition {
		case BaseUnit (
			name: String,
			baseUnit: UnitCategory
		)
		case AliasUnit (
			name: String,
			unit: UnitType
		)
		case NonBaseUnit (
			name: String,
			unit: UnitType,
			fromBase: convertFunction,
			toBase: convertFunction
		)
	}
	private static var definitions = [String: UnitDefinition]()
	
	private static let powers = "⁰¹²³⁴⁵⁶⁷⁸⁹"
	private static func getStringForPower (power: Int) -> String {
		var result = ""
		var sign = ""
		var rpower = power
		if power < 0 { sign = "⁻"; rpower = abs(power) }
		if rpower <= 1 && power >= 0 { return "" }	// handle case of x^1 = x and x^0 = 1
		while rpower > 0 {
			let digit = rpower % 10; rpower /= 10
			let raisedDigit = powers[advance(powers.startIndex, digit)]
			result = [raisedDigit] + result
		}
		return sign + result
	}
	
	var description: String {
		return units.description
	}
	
	//
	// Basic unit definition
	//
	static func defineBaseUnit (name: String, unit: UnitCategory, abbreviation: String) {
		definitions[abbreviation] = .BaseUnit(name: name, baseUnit: unit)
	}

	//
	// Derived unit definition which is defined as function of one of the base units (e.g., feet = k * meters)
	//
	static func defineUnit (name: String, unit: UnitType, abbreviation: String, toBase: convertFunction = { $0 }, fromBase: convertFunction? = nil) {
		let fromFunction : convertFunction
		if let fromBase = fromBase {
			fromFunction = fromBase
		} else {
			fromFunction = { $0/toBase(1) }
		}
		definitions[abbreviation] = .NonBaseUnit(name: name, unit: unit, fromBase:fromFunction, toBase:toBase)
	}
		
	//
	// An alias unit definition creates a pseudo-base unit that is used in place of an SI unit definition
	// For example: the SI Volt derived unit is defined as kg m^2 / (A s^3).  With this defined alias,
	// a "V" will be returned whenever kg m^2 / (A s^3) occurs.
	//
	static func defineAliasUnit (name: String, unit: UnitType, abbreviation: String) {
		definitions[abbreviation] = .AliasUnit(name: name, unit: unit)
	}
	
	private static func defineUnits () {
		// Basic conversion constants
		let inchPerFoot  = 12.0
		let cmPerInch    = 2.54
		let feetPerMile  = 5280.0
		let secsPerMin   = 60.0
		let minsPerHour  = secsPerMin
		let secsPerHour  = secsPerMin * minsPerHour
		let secsPerDay	 = secsPerHour * 24
		let degFOffset   = 32.0
		let centi		 = 1/100.0
		let K			 = 1000.0
		let milli		 = 1/1000.0
		let degFPerdegC  = 9.0/5.0
		let absoluteZero = 273.16
		let kgPerLb		 = 0.4535924
		
		// Define some baseline units where SI units are the baseline
		// By convention the base units come first
		defineBaseUnit("meter",   unit: .Length,		abbreviation: "m")
		defineBaseUnit("second",  unit: .Time,			abbreviation: "s")
		defineBaseUnit("kelvin",  unit: .Temperature,	abbreviation: "K")
		defineBaseUnit("gram",	  unit: .Mass,			abbreviation: "g")
		defineBaseUnit( "ampere", unit: .Current,		abbreviation: "A")
		defineBaseUnit("candela", unit: .Luminance,		abbreviation: "cd")
		defineBaseUnit("mole",	  unit: .Amount,		abbreviation: "mol")
		
		// Add other derived units
		defineMetricUnitsfor("g")  // automatically add all gram-related metric scaled units
		defineMetricUnitsfor("m")  // automatically add all meter-related metric scaled units
		defineMetricUnitsfor("s")  // automatically add all second-related metric scaled units
		defineMetricUnitsfor("A")  // automatically add all current-related metric scaled units
		
		// Only need to define non-metric units
		let M = Units(.Mass).units
		let L = Units(.Length).units
		let T = Units(.Time).units
		let Temp = Units(.Temperature).units
		defineUnit("pound",		 unit: M,	 abbreviation: "lb",  toBase: { $0*kgPerLb*K } )
		defineUnit( "foot",		 unit: L,	 abbreviation: "ft",  toBase: { inchPerFoot*cmPerInch*$0*centi } )
		defineUnit("inch",		 unit: L,	 abbreviation: "in",  toBase: { cmPerInch*$0*centi } )
		defineUnit("mile",		 unit: L,	 abbreviation: "mi",  toBase: { feetPerMile*inchPerFoot*cmPerInch*$0*centi } )
		defineUnit("hour",		 unit: T,	 abbreviation: "hr",  toBase: { $0*secsPerHour } )
		defineUnit("minute",	 unit: T,	 abbreviation: "min", toBase: { $0*secsPerMin } )
		defineUnit("day",		 unit: T,	 abbreviation: "day", toBase: { $0*secsPerDay } )
		defineUnit("week",		 unit: T,	 abbreviation: "wk",  toBase: { $0*secsPerDay*7 } )
		defineUnit("fahrenheit", unit: Temp, abbreviation: "°F",  toBase: { ($0-degFOffset)/degFPerdegC + absoluteZero}, fromBase: { degFPerdegC*($0-absoluteZero)+degFOffset} )
		defineUnit("celsius",	 unit: Temp, abbreviation: "°C",  toBase: { $0+absoluteZero}, fromBase: { $0-absoluteZero } )
	
		// Define some often-used aliases for the SI derived units
		defineAliasUnit("volt",  unit: Units("kg m m / A s s s").units,	  abbreviation: "V")	// Volt
		defineAliasUnit("ohm",   unit: Units("kg m m / A A s s s").units, abbreviation: "Ω")	// Ohm
		defineAliasUnit("joule", unit: Units("kg m m / s s").units,		  abbreviation: "J")	// Joule
		defineAliasUnit("watt",	 unit: Units("kg m m / s s s").units,	  abbreviation: "W")	// Watt
		
		defineAliasUnit("litre",	  unit: Units("m m m").units,		 abbreviation: "l")		// litre
		defineAliasUnit("fahrenheit", unit: Units("°F").units,			 abbreviation: "F")		// alias for °F
		defineAliasUnit("celsius",	  unit: Units("°C").units,			 abbreviation: "C")		// alias for °C
	}
	
	private static func defineMetricUnitsfor (abbreviation: String) {
		if let base = definitions[abbreviation] {
			
			let prefixes = ["exa", "peta", "tera", "giga", "mega", "kilo", "hecto", "deca", "deci", "centi", "milli", "micro", "nano", "pico"]
			let abbrevs  = ["E", "P", "T", "G", "M", "k", "h", "da", "d", "c", "m", "μ", "n", "p"]
			let scale = [1e18, 1e15, 1e12, 1e9, 1e6, 1,000, 100, 10, 1e-1, 1e-2, 1e-3, 1e-6, 1e-9, 1e-12]
			let units : UnitType
			let name : String
			
			switch base {
				case .BaseUnit(let name, let baseType): units = Units(baseType).units
				case .AliasUnit(let name, let units): break
				default: return
			}
			
			for (index, prefix) in enumerate(prefixes) {
				defineUnit(prefix+name, unit: units, abbreviation: abbrevs[index]+abbreviation, toBase: { $0*scale[index] } )
			}
		}
	}
	
	//
	// Initialize with unit abbreviation strings like Units("m / s s") for m/s²
	//
	init (_ unitString : String) {
		units = UnitType()
		
		// define some standard units
		if Units.definitions.count == 0 { Units.defineUnits() }
		
		// break apart the unit string to actual defined units
		let unitArrays = unitString.componentsSeparatedByString("/")
		
		// extract the lower units
		if let lowerArrays = unitArrays.last?.componentsSeparatedByString(" ") where unitArrays.count > 1 {
			for unit in lowerArrays {
				let abbreviation = unit.stringByReplacingOccurrencesOfString(" ", withString: "")
				if let unitDefinition = Units.definitions[abbreviation] {
					switch unitDefinition {
						case let .NonBaseUnit(_, unit, _, _):
							units.lower.combinedWith(unit.upper)
							units.upper.combinedWith(unit.lower)
						case let .AliasUnit(_, unit):
							units.lower.combinedWith(unit.upper)
							units.upper.combinedWith(unit.lower)
						default: break
					}
				}
			}
		}
		
		// extract the upper units
		if let upperArrays = unitArrays.first?.componentsSeparatedByString(" ") {
			for unit in upperArrays {
				let abbreviation = unit.stringByReplacingOccurrencesOfString(" ", withString: "")
				if let unitDefinition = Units.definitions[abbreviation] {
					switch unitDefinition {
					case let .NonBaseUnit(_, unit, _, _):
						units.lower.combinedWith(unit.lower)
						units.upper.combinedWith(unit.upper)
					case let .AliasUnit(_, unit):
						units.lower.combinedWith(unit.lower)
						units.upper.combinedWith(unit.upper)
					default: break
					}
				}
			}
		}
		
		// normalize the units
		units = Units.normalize(units)
	}
	
	//
	// Initialize with the base unit for the UnitCategory
	//
	init (_ unit : UnitCategory) {
		units = UnitType()
		for (abbrev, definition) in enumerate(Units.definitions) {
			switch definition {
				case let .BaseUnit(name, baseUnit):
					if baseUnit == unit { units.upper.add(abbrev) }
				default: break
			}
		}
		
		// normalize the units
		units = Units.normalize(units)
	}
	
	init (_ units : UnitType) {
		self.units = Units.normalize(units)
	}
	
	init (_ upper: UnitBag, _ lower: UnitBag) {
		units = Units.normalize(UnitType(upper: upper, lower: lower))
	}
	
	static func baseUnit (abbreviation: String) -> String {
		if let definition = Units.definitions[abbreviation] {
			for (abbrev, match) in enumerate(Units.definitions) {
				switch definition {
					case let .BaseUnit(name, baseUnit):
						if baseUnit == unit { units.upper.add(abbrev) }
					default: break
				}
			}
		}
		return ""
	}
	
	func abbreviationIsDefined (abbreviation: String) -> Bool {
		return Units.definitions[abbreviation] != nil
	}
	
	private static func convert(number: Double, power: Int, conversion: convertFunction) -> Double {
		var lpower: Int = abs(power)
		var baseNumber = power < 0 ? 1 : number
//		println("convert(\(number)) == \(conversion(number))")
		while lpower > 0 {
			baseNumber = conversion(baseNumber)
			lpower--
		}
		if power < 0 { return number / baseNumber }
		return baseNumber
	}
	
	private static func convert(number: Double, fromType: (String, Int), toType: (String, Int)) -> Double? {
		if let convertToBase = Units.definitions[fromType.0]?.toBase {
//			println("1\(fromType.0) = \(convertToBase(1)) \(Units.baseUnit(fromType.0))")
			var baseNumber = Units.convert(number, power: fromType.1, conversion: convertToBase)
			if let convertToType = Units.definitions[toType.0]?.fromBase {
//				println("1\(toType.0) = \(convertToType(1)) \(Units.baseUnit(toType.0))")
				return Units.convert(baseNumber, power: toType.1, conversion: convertToType)
			}
		}
		return nil
	}
	
	private func getMatchingUnit (unit: String, inUnits: UnitBag) -> (String, Int)? {
		for funit in inUnits {
			if Units.baseUnit(unit) == Units.baseUnit(funit.0) { return funit }
		}
		return nil
	}
	
	static func convert (number: Double, fromType: Units, toType: Units) -> Double? {
		if fromType.isCompatibleWith(toType) {
			var x = number
			for unit in fromType.units.upper {
				if let toUnit = fromType.getMatchingUnit(unit.0, inUnits: toType.units.upper) {
					if let result = Units.convert(x, fromType: unit, toType: toUnit) {
						x = result
					} else {
						return nil
					}
				}
			}
			for unit in fromType.units.lower {
				if let toUnit = fromType.getMatchingUnit(unit.0, inUnits: toType.units.lower) {
					// Note: Unit powers are negated to give an inverse conversion
					if let result = Units.convert(x, fromType: (unit.0, -unit.1), toType: (toUnit.0, -toUnit.1)) {
						x = result
					} else {
						return nil
					}
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
		return (self.baseUnit == unit.baseUnit)
	}
	
	static private func normalize (units: UnitType) -> UnitType {
		// if any units are common to both upper and lower units they cancel each other
		let common = units.upper.itemsAlsoIn(units.lower)
		return UnitType(upper: units.upper.removeItemsIn(common), lower: units.lower.removeItemsIn(common))
	}
	
	func inverse () -> Units {
		// invert units by swapping upper/lower units (i.e., m/s² => s²/m)
		return Units(units.lower, units.upper)
	}
	
	func div (x: Units) -> Units {
		// divide the units (i.e., m/s / s => m/s²)
		return x.inverse().mul(self)
	}
	
	func mul (x: Units) -> Units {
		var result = self.units
		result.upper = result.upper.combinedWith(x.units.upper)
		result.lower = result.lower.combinedWith(x.units.lower)
		return Units(result)
	}

}

// required for UnitType Hashable protocol
func == (lhs: Units, rhs: Units) -> Bool {
	return lhs.units.upper == rhs.units.upper && lhs.units.lower == rhs.units.lower
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

