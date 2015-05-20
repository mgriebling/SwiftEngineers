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
//		let equations = Matrix([13, -8, -3, -8, 10, -1, -3, -1, 11], numberOfRows: 3)
//		if let answer = Equations.solveUsingCramersRule(equations, y: Vector([20, -5, 0])) {
//			println("Solution 1 = \(answer)")
//		} else {
//			println("Cramer solution didn't work!")
//		}
//		
//		let equation2 = Matrix([1, 1, 1, 2, 1, -1, 3, 1, -3], numberOfRows: 3)
//		if let answer = Equations.solveUsingGaussianElimination(equation2, y: Vector([6, 1, -4])) {
//			println("Solution 2 = \(answer)")
//		}
//		if let answer = Matrix.solveLinear1([13, -8, -3, -8, 10, -1, -3, -1, 11], n: [20, -5, 0]) {
//			println("Solution 1 = \(answer)")
//		}
//		if let answer = Matrix.solveLinear2([10, -3,  5, -7,  2, -1, 0,  6,  5], n: [7, 4, 6]) {
//			println("Solution 2 = \(answer)")
//		}
//		if let coefs = Equations.curveFit(Vector([0, 0.5, 1.0, 1.5, 2.0, 2.5]),
//											y: Vector([0.0674, -0.9156, 1.6253, 3.0377, 3.3535, 7.9409]), order: 2) {
//			println("Curve fit = \(coefs)")
//		}
		let x = BigReal(0.1)
		var y : qd
	}

	override func didReceiveMemoryWarning() {
		super.didReceiveMemoryWarning()
		// Dispose of any resources that can be recreated.
	}


}

