//
//  ViewController.swift
//  EngineerMath
//
//  Created by Michael Griebling on 22May2015.
//  Copyright (c) 2015 Solinst Canada. All rights reserved.
//

import Cocoa

class ViewController: NSViewController {

	override func viewDidLoad() {
		super.viewDidLoad()

		// Do any additional setup after loading the view.
//		var a = NSDecimalNumber(string: "12.345E50")
//		println("a = \(a), \nmax = \(NSDecimalNumber.maximumDecimalNumber()), \nmin = \(NSDecimalNumber.minimumDecimalNumber())")
//		let b = NSDecimalNumber.one().decimalNumberByDividingBy(a)
//		println("1/a = \(b)")
		
		// BigCFloat
////		var x : BigReal = 1
////		var x = BigReal(1)
//		var x : BigComplex = 1
//		var y : BigComplex = 2.3456-2.5*i
//		var z : BigComplex = "+5 - i23"
		
////		println("atan \(y)/\(x) = \(x.atan2(y))")
//		println("x = \(x), y = \(y)")
//		println("z = \(z); 1/z = \(1/z)")
//		println("\(x) + \(y) = \(x+y)")
//		println("\(x) - \(y) = \(x-y)")
//		println("\(x) * \(y) = \(x*y)")
//		println("\(x) / \(y) = \(x/y)")
//		println("\(x) ** 2 = \(x ** 2)")
//		println("\(x) ** 3 = \(x ** 3)")
//		println("\(y) ** 2 = \(y ** 2)")
//		println("\(y) ** 3 = \(y ** 3)")
//		println("\(x) ** -1 = \(x ** -1)")
//		println("2 ** 1000 = \(2 ** 1000)")
//		if x > y { println("\(x) > \(y)") }
//		else if x < y { println("\(x) < \(y)") }
//		else { println("\(x) = \(y)") }
		
//		var t = UnitBag()
		
//		t.add("m"); t.add("m")
//		t.add("ft")
////		t.removeAll("m")
//		
//		println("Bag contains \(t.count) items: ")
//		for (index, item) in enumerate(t) {
//			print("\(item)")
//			if index < t.uniqueCount-1 { print(", ") }
//		}
//		println()
		
		// Test unit conversions
		let hour = Units("hr")
		let miphr = Units("mi/hr")
		let kmphr = Units("km/hr")
		let mps = Units("m/s")
		let ftps = Units("ft/s")
		let seconds = Units("s")
		let mile = Units("mi")
		let km = Units("km")
		let degreeC = Units("°C")
		let degreeF = Units("°F")
		let kelvin = Units("K")
		let kg = Units("kg")
		let lb = Units("lb")
		let mA = Units("mA")
		let Amp = Units("A")
		let volt = Units("V")
		let mV = Units("mV")
		let lightYear = Units("ly")
//		let c = Units("c")
		var result = Units.convert(10, fromType: hour, toType: seconds)
		print("10 \(hour) = \(result!) \(seconds)")
		result = Units.convert(10, fromType:mile, toType: km)
		print("10 \(mile) = \(result!) \(km)")
		result = Units.convert(60, fromType:miphr, toType: kmphr)
		print("60 \(miphr) = \(result!) \(kmphr)")
		result = Units.convert(5, fromType:mps, toType: kmphr)
		print("5 \(mps) = \(result!) \(kmphr)")
		result = Units.convert(5, fromType:ftps, toType: kmphr)
		print("5 \(ftps) = \(result!) \(kmphr)")
		result = Units.convert(25, fromType:degreeC, toType: kelvin)
		print("25 \(degreeC) = \(result!) \(kelvin)")
		result = Units.convert(25, fromType:degreeC, toType: degreeF)
		print("25 \(degreeC) = \(result!) \(degreeF)")
		result = Units.convert(200, fromType:lb, toType: kg)
		print("200 \(lb) = \(result!) \(kg)")
		print("200 \(mA) = \(Units.convert(200, fromType:mA, toType: Amp)!) \(Amp)")
		print("200 \(mV) = \(Units.convert(200, fromType:mV, toType: volt)!) \(volt)")
		print("V/A = \(volt/Amp)")
		print("1 \(lightYear) = \(Units.convert(1, fromType:lightYear, toType: km)!) \(km)")
//		print("1 \(c) = \(Units.convert(1, fromType:c, toType: kmphr)!) \(kmphr)")
        print("unsigned long size = \(sizeof(Int))")
	}

	override var representedObject: AnyObject? {
		didSet {
		// Update the view, if already loaded.
		}
	}


}

