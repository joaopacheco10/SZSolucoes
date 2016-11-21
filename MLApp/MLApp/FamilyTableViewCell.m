//
//  FamilyTableViewCell.m
//  MLApp
//
//  Created by Joao Pacheco on 2/4/15.
//  Copyright (c) 2015 SZ Solucoes. All rights reserved.
//

#import "FamilyTableViewCell.h"

@implementation FamilyTableViewCell

@synthesize familyLabel;
@synthesize valPerc;
@synthesize valFam;

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
