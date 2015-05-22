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
		BigReal.bitSize = 256
		var x = BigReal(1234567890)
		var y = BigReal("456.789")
		
		println("Tests using \(BigReal.bitSize) bits of resolution");
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

