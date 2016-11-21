//
//  AgreeTableViewCell.m
//  MLApp
//
//  Created by Joao Pacheco on 14/10/16.
//  Copyright © 2016 SZ Solucoes. All rights reserved.
//

#import "AgreeTableViewCell.h"

@implementation AgreeTableViewCell

@synthesize lblArea;
@synthesize lblAgree;
@synthesize lblDtPay;
@synthesize lblValue;
@synthesize lblAction;
@synthesize lblClient;
@synthesize lblRepres;


- (id)initWithStyle:(UITableViewCellStyle)style reuseIdentifier:(NSString *)reuseIdentifier
{
    self = [super initWithStyle:style reuseIdentifier:reuseIdentifier];
    if (self) {
        // Initialization code
    }
    return self;
}


- (void)awakeFromNib {
    [super awakeFromNib];
    // Initialization code
}

- (void)setSelected:(BOOL)selected animated:(BOOL)animated {
    [super setSelected:selected animated:animated];

    // Configure the view for the selected state
}

@end
