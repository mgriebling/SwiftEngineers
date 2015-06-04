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
//		var x : BigReal = 1
//		var x = BigReal(1)
		var a : BigReal = +5.5
		var x : BigComplex = 1 + 5.i
		var y : BigComplex = 2.3456-2.5*i
		var z = BigComplex("+5 - i23")
		
//		println("atan \(y)/\(x) = \(x.atan2(y))")
		println("x = \(x), y = \(y)")
		println("z = \(z); 1/z = \(1/z)")
		println("\(x) + \(y) = \(x+y)")
		println("\(x) - \(y) = \(x-y)")
		println("\(x) * \(y) = \(x*y)")
		println("\(x) / \(y) = \(x/y)")
		println("\(x) ** 2 = \(x ** 2)")
		println("\(x) ** 3 = \(x ** 3)")
		println("\(y) ** 2 = \(y ** 2)")
		println("\(y) ** 3 = \(y ** 3)")
		println("\(x) ** -1 = \(x ** -1)")
		println("2 ** 1000 = \(2 ** 1000)")
		if x > y { println("\(x) > \(y)") }
		else if x < y { println("\(x) < \(y)") }
		else { println("\(x) = \(y)") }
	}

	override var representedObject: AnyObject? {
		didSet {
		// Update the view, if already loaded.
		}
	}


}

