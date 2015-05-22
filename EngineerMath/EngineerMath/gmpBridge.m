//
//  gmpBridge.m
//  EngineerMath
//
//  Created by Michael Griebling on 22May2015.
//  Copyright (c) 2015 Solinst Canada. All rights reserved.
//

#import "gmpBridge.h"

@implementation gmp

+ (NSInteger) getBitSize {
	return mpf_get_default_prec();
}

+ (void) setBitSize: (NSInteger)digits {
	mpf_set_default_prec(digits);
}

+ (void) setNumber: (mpf_t *)number fromString: (NSString *)string {
	mpf_init_set_str(*number, string.UTF8String, 10);
}

+ (void) setNumber: (mpf_t *)number fromInt: (NSInteger)integer {
	mpf_init_set_si(*number, integer);
}

+ (void) setNumber: (mpf_t *)number fromDouble: (double)x {
	mpf_init_set_d(*number, x);
}

+ (void) setNumber: (mpf_t *)number fromLongDouble: (mpf_t)x {
	mpf_init_set(*number, x);
}

+ (NSString *) getStringFrom: (const mpf_t)number {
	NSMutableString *str;
	char cstr[1024];
	mp_exp_t exp;
	mpf_get_str(cstr, &exp, 10, 50, number);
	str = [NSMutableString stringWithUTF8String:cstr];
	if (exp < 0 || exp > 100) { return [NSString stringWithFormat:@"0.%@e%li", str, exp]; }
	if (str.length < exp) { str = [NSMutableString stringWithString:[str stringByPaddingToLength:exp+1 withString:@"0" startingAtIndex:0]]; }
	[str insertString:@"." atIndex:exp];
	return str;
}

+ (double) getDoubleFrom: (const mpf_t)number {
	return mpf_get_d(number);
}

+ (NSInteger) getIntegerFrom: (const mpf_t)number {
	return mpf_get_si(number);
}

+ (void) add: (mpf_t)x toNumber: (mpf_t)y giving: (mpf_t*)result {
	mpf_init(*result);
	mpf_add(*result, x, y);
}

+ (void) sub: (mpf_t)x toNumber: (mpf_t)y giving: (mpf_t*)result {
	mpf_init(*result);
	mpf_sub(*result, x, y);
}

+ (void) mul: (mpf_t)x toNumber: (mpf_t)y giving: (mpf_t*)result {
	mpf_init(*result);
	mpf_mul(*result, x, y);
}

+ (void) div: (mpf_t)x toNumber: (mpf_t)y giving: (mpf_t*)result {
	mpf_init(*result);
	mpf_div(*result, x, y);
}

+ (void) sqrt: (mpf_t)x giving: (mpf_t*)result {
	mpf_init(*result);
	mpf_sqrt(*result, x);
}

+ (void) negate: (mpf_t)x giving: (mpf_t*)result {
	mpf_init(*result);
	mpf_neg(*result, x);
}

+ (void) abs: (mpf_t)x giving: (mpf_t*)result {
	mpf_init(*result);
	mpf_abs(*result, x);
}

+ (void) ipower:(mpf_t)x toPower:(NSInteger)power giving: (mpf_t*)result {
	mpf_init(*result);
	if (power >= 0) {
		mpf_pow_ui(*result, x, power);
	} else {
		mpf_pow_ui(*result, x, -power);
		mpf_ui_div(*result, 1, *result);
	}
}

+ (NSComparisonResult)cmp:(mpf_t)x toNumber:(mpf_t)y {
	NSInteger result = mpf_cmp(x, y);
	if (result < 0) {
		return NSOrderedAscending;
	} else if (result > 0) {
		return NSOrderedDescending;
	}
	return NSOrderedSame;
}

@end
