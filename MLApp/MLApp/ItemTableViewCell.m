//
//  ItemTableViewCell.m
//  MLApp
//
//  Created by SZ Solucoes on 16/04/14.
//  Copyright (c) 2014 SZ Solucoes. All rights reserved.
//

#import "ItemTableViewCell.h"

@implementation ItemTableViewCell

@synthesize lblDesc;
@synthesize lblItemCode;
@synthesize lblQuantity;
@synthesize lblValue;
@synthesize lblType;
@synthesize lblValPerc;
@synthesize lblTotal;

- (id)initWithStyle:(UITableViewCellStyle)style reuseIdentifier:(NSString *)reuseIdentifier
{
    self = [super initWithStyle:style reuseIdentifier:reuseIdentifier];
    if (self) {
        // Initialization code
        
        self.contentView.autoresizingMask = UIViewAutoresizingFlexibleHeight|UIViewAutoresizingFlexibleWidth;
    }
    return self;
}

- (void)awakeFromNib {
    [super awakeFromNib];
    // Initialization code
}

- (void)setSelected:(BOOL)selected animated:(BOOL)animated
{
    [super setSelected:selected animated:animated];

    // Configure the view for the selected state
}

@end
