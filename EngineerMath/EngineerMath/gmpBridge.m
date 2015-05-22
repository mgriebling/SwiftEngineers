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
	if (str.length <= exp) { return [NSString stringWithFormat:@"%@e%li", str, exp]; }
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

@end
