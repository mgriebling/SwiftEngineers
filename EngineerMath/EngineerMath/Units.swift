//
//  Units.swift
//  DimensionalTests
//
//  Created by Michael Griebling on 10Apr2015.
//  Copyright (c) 2015 Solinst Canada. All rights reserved.
//

import Foundation

class Units : Printable, Equatable {

	typealias UnitBag = Bag<String>
	
	//
	// Units are primarily defined within this type
	//
	struct UnitType {
		var upper = UnitBag()	// units to positive power
		var lower = UnitBag()	// units to negative power
		
		private static let rdot = "∙"
		var description: String {
			var unitString = ""
			for (abbrev, power) in upper {
				if !unitString.isEmpty { unitString += UnitType.rdot }
				unitString += abbrev + Units.getStringForPower(power)
			}
			for (abbrev, power)  in lower {
				if !unitString.isEmpty { unitString += UnitType.rdot  }
				unitString += abbrev + Units.getStringForPower(-power)
			}
			return unitString
		}
		
		init (abbreviation: String) {
			upper.add(abbreviation)
		}
		
		init (upper: UnitBag, lower: UnitBag) {
			self.upper = upper
			self.lower = lower
		}
		
		init () {}
		
		init (_ units: String) {
			// break apart the unit string to actual defined units
			let unitArrays = units.componentsSeparatedByString("/")
			
			// extract the lower units
			if let lowerArrays = unitArrays.last?.componentsSeparatedByString(" ") where unitArrays.count > 1 {
				for unit in lowerArrays {
					let abbreviation = unit.stringByReplacingOccurrencesOfString(" ", withString: "")
					if Units.abbreviationIsDefined(abbreviation) {
						lower.add(abbreviation)
					}
				}
			}
			
			// extract the upper units
			if let upperArrays = unitArrays.first?.componentsSeparatedByString(" ") {
				for unit in upperArrays {
					let abbreviation = unit.stringByReplacingOccurrencesOfString(" ", withString: "")
					if Units.abbreviationIsDefined(abbreviation) {
						upper.add(abbreviation)
					}
				}
			}
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
			
			// find if the base unit can be converted to a derived unit (e.g., m/s -> mps)
//			for unit in Units.definitions {
//				switch unit {
//				case let .AliasUnit(_, baseItem):
//					if base.isEqualTo(baseItem) {
//						base = UnitType(u)
//					}
//				default: break
//				}
//			}
			return base
		}
		
		func convertToBaseUnits (x: Double) -> Double {
			var number = x
			for (abbrev, power) in upper {
				switch Units.definitions[abbrev]! {
					case let .NonBaseUnit(_, _, _, convertToBase):
						number = Units.convert(number, power: power, conversion: convertToBase)
					default: break
				}
			}
			for (abbrev, power) in lower {
				switch Units.definitions[abbrev]! {
					case let .NonBaseUnit(_, _, _, convertToBase):
						number = Units.convert(number, power: -power, conversion: convertToBase)
					default: break
				}
			}
			return number
		}
		
		func convertFromBaseUnits (x: Double) -> Double {
			var number = x
			for (abbrev, power) in upper {
				switch Units.definitions[abbrev]! {
					case let .NonBaseUnit(_, _, convertToUnits, _):
						number = Units.convert(number, power: power, conversion: convertToUnits)
					default: break
				}
			}
			for (abbrev, power) in lower {
				switch Units.definitions[abbrev]! {
					case let .NonBaseUnit(_, _, convertToUnits, _):
						number = Units.convert(number, power: -power, conversion: convertToUnits)
					default: break
				}
			}
			return number
		}
		
		func isEqualTo (x: UnitType) -> Bool {
			return upper == x.upper && lower == x.lower
		}
		
		mutating func add (x: UnitType) {
			upper = upper.combinedWith(x.upper)
			lower = lower.combinedWith(x.lower)
		}
		
		func inverse () -> UnitType {
			return UnitType(upper: lower, lower: upper)
		}
	}
	
	typealias convertFunction = (Double) -> Double
	
	// contains all the aggregated units e.g., m∙s⁻¹
	private var units: UnitType
	
	// Basic unit definitions
	enum UnitDefinition {
		case BaseUnit (
			name: String
		)
		case AliasUnit (
			name: String,
			baseUnits: UnitType
		)
		case NonBaseUnit (
			name: String,
			baseUnit: String,			// base Unit index (either BaseUnit or AliasUnit)
			fromBase: convertFunction,
			toBase: convertFunction
		)
	}
	private static var definitions = [String: UnitDefinition]()
	
	private static let powers = "⁰¹²³⁴⁵⁶⁷⁸⁹"
	private static func getStringForPower (power: Int) -> String {
		var result = ""
		let sign = power < 0 ? "⁻" : ""
		var rpower = abs(power)
		if rpower <= 1 && power >= 0 { return "" }	// handle case of x^1 = x and x^0 = 1
		while rpower > 0 {
			let digit = rpower % 10; rpower /= 10
			let raisedDigit = powers[advance(powers.startIndex, digit)]
			result.insert(raisedDigit, atIndex: result.startIndex)
		}
		return sign + result
	}
	
	var description: String {
		return units.description
	}
	
	//
	// Basic unit definition
	//
	static func defineBaseUnit (name: String, abbreviation: String) {
		definitions[abbreviation] = .BaseUnit(name: name)
	}

	//
	// Derived unit definition which is defined as function of one of the base units (e.g., feet = k * meters)
	//
	static func defineUnit (name: String, base: String, abbreviation: String, toBase: convertFunction = { $0 }, fromBase: convertFunction? = nil) {
		let fromFunction : convertFunction
		if let fromBase = fromBase {
			fromFunction = fromBase
		} else {
			fromFunction = { $0/toBase(1) }
		}
		definitions[abbreviation] = .NonBaseUnit(name: name, baseUnit: base, fromBase:fromFunction, toBase:toBase)
	}
		
	//
	// An alias unit definition creates a pseudo-base unit that is used in place of an SI unit definition
	// For example: the SI Volt derived unit is defined as kg m^2 / (A s^3).  With this defined alias,
	// a "V" will be returned whenever kg m^2 / (A s^3) occurs.
	//
	static func defineAliasUnit (name: String, unit: UnitType, abbreviation: String) {
		definitions[abbreviation] = .AliasUnit(name: name, baseUnits: unit)
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
		let secsPerYear  = secsPerDay * 365.25		// Julian year (exact)
		let FOff		 = 32.0
		let centi		 = 1/100.0
		let K			 = 1000.0
		let degFPerdegC  = 9.0/5.0
		let zeroK		 = 273.15
		let kgPerLb		 = 0.4535924
		let cPermps		 = 299_792_458.0			// m / s (exact)
		let light_year	 = secsPerYear * cPermps	// meters (exact)
		
		// Define some baseline units where SI units are the baseline
		// By convention the base units come first
		let M = "g"
		let L = "m"
		let T = "s"
		let TK = "K"
		let A = "A"
		defineBaseUnit("meter",   abbreviation: L)
		defineBaseUnit("second",  abbreviation: T)
		defineBaseUnit("kelvin",  abbreviation: TK)
		defineBaseUnit("gram",	  abbreviation: M)
		defineBaseUnit("ampere",  abbreviation: A)
		defineBaseUnit("candela", abbreviation: "cd")
		defineBaseUnit("mole",	  abbreviation: "mol")
		
		// Add other derived units
		defineMetricUnitsfor(M)  // automatically add all gram-related metric scaled units
		defineMetricUnitsfor(L)  // automatically add all meter-related metric scaled units
		defineMetricUnitsfor(T)  // automatically add all second-related metric scaled units
		defineMetricUnitsfor(A)  // automatically add all current-related metric scaled units
		
		// Only need to define non-metric units
		defineUnit("pound",		  base: M,     abbreviation: "lb",  toBase: { $0*kgPerLb*K } )
		defineUnit("foot",		  base: L,     abbreviation: "ft",  toBase: { inchPerFoot*cmPerInch*$0*centi } )
		defineUnit("inch",		  base: L,     abbreviation: "in",  toBase: { cmPerInch*$0*centi } )
		defineUnit("mile",		  base: L,     abbreviation: "mi",  toBase: { feetPerMile*inchPerFoot*cmPerInch*$0*centi } )
		defineUnit("hour",		  base: T,     abbreviation: "hr",  toBase: { $0*secsPerHour } )
		defineUnit("minute",	  base: T,     abbreviation: "min", toBase: { $0*secsPerMin } )
		defineUnit("day",		  base: T,     abbreviation: "day", toBase: { $0*secsPerDay } )
		defineUnit("week",		  base: T,     abbreviation: "wk",  toBase: { $0*secsPerDay*7 } )
		defineUnit("year",		  base: T,     abbreviation: "y",   toBase: { $0*secsPerYear } )
		defineUnit("fahrenheit",  base: TK,    abbreviation: "°F",  toBase: { ($0-FOff)/degFPerdegC+zeroK}, fromBase: { degFPerdegC*($0-zeroK)+FOff} )
		defineUnit("celsius",	  base: TK,	   abbreviation: "°C",  toBase: { $0+zeroK}, fromBase: { $0-zeroK } )
		defineUnit("fahrenheit",  base: TK,    abbreviation: "F",   toBase: { ($0-FOff)/degFPerdegC+zeroK}, fromBase: { degFPerdegC*($0-zeroK)+FOff} )
		defineUnit("celsius",	  base: TK,	   abbreviation: "C",   toBase: { $0+zeroK}, fromBase: { $0-zeroK } )
	
		// Define some often-used aliases for the SI derived units
		let V = "V"
		let ohm = "Ω"
		let J = "J"
		let W = "W"
		let l = "l"
		let mps = "mps"
		defineAliasUnit("volt",		   unit: UnitType("kg m m / A s s s"),   abbreviation: V)		// Volt
		defineAliasUnit("ohm",		   unit: UnitType("kg m m / A A s s s"), abbreviation: ohm)		// Ohm
		defineAliasUnit("joule",	   unit: UnitType("kg m m / s s"),		 abbreviation: J)		// Joule
		defineAliasUnit("watt",		   unit: UnitType("kg m m / s s s"),	 abbreviation: W)		// Watt
		defineAliasUnit("litre",	   unit: UnitType("m m m"),				 abbreviation: l)		// litre
		defineAliasUnit("speed",	   unit: UnitType("m / s"),			     abbreviation: mps)		// meter/second
		
		defineMetricUnitsfor(V)		// automatically add all volt-related metric scaled units
		defineMetricUnitsfor(ohm)	// automatically add all ohm-related metric scaled units
		defineMetricUnitsfor(J)		// automatically add all joule-related metric scaled units
		defineMetricUnitsfor(W)		// automatically add all watt-related metric scaled units
		defineMetricUnitsfor(l)		// automatically add all litre-related metric scaled units
		
		// non-standard units for derived quantities
		defineUnit("speed of light", base: mps, abbreviation: "c",  toBase: { $0*cPermps } )
		defineUnit("light-year",     base: L,	abbreviation: "ly", toBase: { $0*light_year } )
	}
	
	private static func defineMetricUnitsfor (abbreviation: String) {
		if let base = definitions[abbreviation] {
			
			let prefixes = ["exa", "peta", "tera", "giga", "mega", "kilo", "hecto", "deca", "deci", "centi", "milli", "micro", "nano", "pico"]
			let abbrevs  = ["E", "P", "T", "G", "M", "k", "h", "da", "d", "c", "m", "μ", "n", "p"]
			let scale = [1e18, 1e15, 1e12, 1e9, 1e6, 1000, 100, 10, 1e-1, 1e-2, 1e-3, 1e-6, 1e-9, 1e-12]
			let name : String
			
			switch base {
				case let .BaseUnit(lname): name = lname
				case let .AliasUnit(lname, _): name = lname
				default: return  // abort - not a base or alias unit
			}
			
			for (index, prefix) in enumerate(prefixes) {
				defineUnit(prefix+name, base:abbreviation, abbreviation: abbrevs[index]+abbreviation, toBase: { $0*scale[index] } )
			}
		}
	}
	
	//
	// Initialize with unit abbreviation strings like Units("m / s s") for m/s²
	//
	init (_ unitString : String) {
		// define some standard units
		if Units.definitions.count == 0 { Units.defineUnits() }
		
		// define the new unit type
		units = UnitType(unitString)
		
		// normalize the units
		units = Units.normalize(units)
	}
	
	init (_ units : UnitType) {
		self.units = Units.normalize(units)
	}
	
	private init (_ upper: UnitBag, _ lower: UnitBag) {
		units = Units.normalize(UnitType(upper: upper, lower: lower))
	}
	
	static func baseUnit (abbreviation: String) -> String {
		if let definition = Units.definitions[abbreviation] {
			switch definition {
			case let .NonBaseUnit(_, baseUnit, _, _): return baseUnit
				case .AliasUnit(_, _): return abbreviation
				case .BaseUnit(_): return abbreviation
			}
		}
		return ""
	}
	
	static func abbreviationIsDefined (abbreviation: String) -> Bool {
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
	
	static func convert (number: Double, fromType: Units, toType: Units) -> Double? {
		if fromType.isCompatibleWith(toType) {
			let x = fromType.units.convertToBaseUnits(number)
			return toType.units.convertFromBaseUnits(x)
		}
		return nil
	}
	
	func convert (number: Double, toUnit: Units) -> Double? {
		return Units.convert(number, fromType: self, toType: toUnit)
	}
	
	func isCompatibleWith (unit: Units) -> Bool {
		return (self.units.baseUnit.isEqualTo(unit.units.baseUnit))
	}
	
	static private func normalize (units: UnitType) -> UnitType {
		// if any units are common to both upper and lower units they cancel each other
		let common = units.upper.itemsAlsoIn(units.lower)
		return UnitType(upper: units.upper.removeItemsIn(common), lower: units.lower.removeItemsIn(common))
	}
	
	func inverse () -> Units {
		// invert units by swapping upper/lower units (i.e., m/s² => s²/m)
		return Units(units.inverse())
	}
	
	func div (x: Units) -> Units {
		// divide the units (i.e., m/s / s => m/s²)
		return x.inverse().mul(self)
	}
	
	func mul (x: Units) -> Units {
		var result = self.units
		result.add(x.units)
		return Units(result)
	}

}

// required for UnitType Hashable protocol
func == (lhs: Units, rhs: Units) -> Bool {
	return lhs.units.isEqualTo(rhs.units)
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

