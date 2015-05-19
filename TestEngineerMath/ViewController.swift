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
			println("Solution 1 = \(answer)")
		} else {
			println("Cramer solution didn't work!")
		}
		
		let equation2 = Matrix([1, 1, 1, 2, 1, -1, 3, 1, -3], numberOfRows: 3)
		if let answer = Equations.solveUsingGaussianElimination(equation2, y: Vector([6, 1, -4])) {
			println("Solution 2 = \(answer)")
		}
	}

	override func didReceiveMemoryWarning() {
		super.didReceiveMemoryWarning()
		// Dispose of any resources that can be recreated.
	}


}

