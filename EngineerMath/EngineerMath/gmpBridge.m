//
//  gmpBridge.m
//  EngineerMath
//
//  Created by Michael Griebling on 22May2015.
//  Copyright (c) 2015 Solinst Canada. All rights reserved.
//

#import "gmpBridge.h"

@implementation gmp

+ (NSInteger) getDigits {
	return mpf_get_default_prec();
}

+ (void) setDigits: (NSInteger)digits {
	mpf_set_default_prec(digits);
}

+ (void) setNumber: (mpf_t *)number fromString: (NSString *)string {
	mpf_init_set_str(*number, string.UTF8String, 10);
}

+ (NSString *) getStringFrom: (mpf_t)number {
	NSMutableString *str;
	char cstr[1024];
	mp_exp_t exp;
	mpf_get_str(cstr, &exp, 10, 50, number);
	str = [NSMutableString stringWithUTF8String:cstr];
	[str insertString:@"." atIndex:exp];
	return str;
}

+ (void) add: (mpf_t)x toNumber: (mpf_t)y giving: (mpf_t*)result {
	mpf_add(*result, x, y);
}

+ (void) sub: (mpf_t)x toNumber: (mpf_t)y giving: (mpf_t*)result {
	mpf_sub(*result, x, y);
}

+ (void) mul: (mpf_t)x toNumber: (mpf_t)y giving: (mpf_t*)result {
	mpf_mul(*result, x, y);
}

+ (void) div: (mpf_t)x toNumber: (mpf_t)y giving: (mpf_t*)result {
	mpf_div(*result, x, y);
}

@end
