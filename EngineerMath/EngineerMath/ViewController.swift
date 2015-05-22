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
		var x = mpf_t()
		var y = mpf_t()
		var z = mpf_t()
		
		gmp.setDigits(256)
		gmp.setNumber(&x, fromString: "123.456")
		gmp.setNumber(&y, fromString: "456.789")
		gmp.setNumber(&z, fromString: "0")
		println("Tests using \(gmp.getDigits()) bits of resolution");
		gmp.add(&x, toNumber: &y, giving: &z)
		println(gmp.getStringFrom(&x)! + " + " + gmp.getStringFrom(&y)! + " = " + gmp.getStringFrom(&z)!)
		gmp.sub(&x, toNumber: &y, giving: &z)
		println(gmp.getStringFrom(&x)! + " - " + gmp.getStringFrom(&y)! + " = " + gmp.getStringFrom(&z)!)
		gmp.mul(&x, toNumber: &y, giving: &z)
		println(gmp.getStringFrom(&x)! + " * " + gmp.getStringFrom(&y)! + " = " + gmp.getStringFrom(&z)!)
		gmp.div(&x, toNumber: &y, giving: &z)
		println(gmp.getStringFrom(&x)! + " / " + gmp.getStringFrom(&y)! + " = " + gmp.getStringFrom(&z)!)
	}

	override var representedObject: AnyObject? {
		didSet {
		// Update the view, if already loaded.
		}
	}


}

