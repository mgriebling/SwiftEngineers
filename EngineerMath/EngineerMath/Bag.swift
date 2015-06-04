//
//  Bag.swift
//  EngineerMath
//
//  Created by Michael Griebling on 3Jun2015.
//  Copyright (c) 2015 Solinst Canada. All rights reserved.
//

import Foundation

struct Bag<T:Hashable> {
	
	var items = [T: Int]()	// Bag definition as an object and a quantity
	
	var count : Int {
		return items.count
	}
	
	func add (item: T) {
		
	}
}