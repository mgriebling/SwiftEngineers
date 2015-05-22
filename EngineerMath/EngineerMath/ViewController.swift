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
		gmp.setBitSize(256)
		var x = BigReal("1e20")
		var y = BigReal("456.789")
		var z = BigReal()
		
		println("Tests using \(gmp.getBitSize()) bits of resolution");
		println("\(x) + \(y) = \(x+y)")
		println("\(x) - \(y) = \(x-y)")
		println("\(x) * \(y) = \(x*y)")
		println("\(x) / \(y) = \(x/y)")
	}

	override var representedObject: AnyObject? {
		didSet {
		// Update the view, if already loaded.
		}
	}


}

