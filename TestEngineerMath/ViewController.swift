//
//  ViewController.swift
//  TestEngineerMath
//
//  Created by Mike Griebling on 17 May 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import UIKit

class ViewController: UIViewController {

	override func viewDidLoad() {
		super.viewDidLoad()
		// Do any additional setup after loading the view, typically from a nib.
		let equations = Matrix([13, -8, -3, -8, 10, -1, -3, -1, 11], numberOfRows: 3)
		if let answer = Equations.solveUsingCramersRule(equations, y: Vector([20, -5, 0])) {
			println("Solution = \(answer)")
		} else {
			println("Cramer solution didn't work!")
		}
	}

	override func didReceiveMemoryWarning() {
		super.didReceiveMemoryWarning()
		// Dispose of any resources that can be recreated.
	}


}

