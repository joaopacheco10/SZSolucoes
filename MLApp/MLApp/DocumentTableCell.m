//
//  DocumentTableCell.m
//  MLApp
//
//  Created by SZ Solucoes on 05/04/14.
//  Copyright (c) 2014 SZ Solucoes. All rights reserved.
//

#import "DocumentTableCell.h"

@implementation DocumentTableCell

@synthesize documentoLabel;
@synthesize valorLabel;
@synthesize origemLabel;
@synthesize dataLabel;
@synthesize fornecLabel;
@synthesize obsLabel;

- (id)initWithStyle:(UITableViewCellStyle)style reuseIdentifier:(NSString *)reuseIdentifier
{
    self = [super initWithStyle:style reuseIdentifier:reuseIdentifier];
    if (self) {
        // Initialization code
    }
    return self;
}

- (void)awakeFromNib
{
    [super awakeFromNib];
    // Initialization code
}



@end
