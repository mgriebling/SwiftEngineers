//
//  Units.swift
//  DimensionalTests
//
//  Created by Michael Griebling on 10Apr2015.
//  Copyright (c) 2015 Solinst Canada. All rights reserved.
//

import Foundation

class Units {
	
	typealias convertFunction = (Double) -> Double
	private var baseUnit: Int
	private var activeUnit: Int
	private static var conversions = [[convertFunction]]()
	private static var names = [[String]]()
	
	var description: String {
		return Units.names[self.baseUnit][self.activeUnit]
	}
	
	init (base: Int, active: Int, baseConversion: convertFunction, name: String) {
		baseUnit = base
		activeUnit = active
		Units.conversions[base][active] = baseConversion
		Units.names[base][active] = name
	}

}


let meters		= Units(base: 0, active: 0, baseConversion: { return $0 }, name: "m")
let millimeters = Units(base: 0, active: 1, baseConversion: { return $0/1000 }, name: "mm")

