//
//  FamilyTableViewCell.h
//  MLApp
//
//  Created by Joao Pacheco on 2/4/15.
//  Copyright (c) 2015 SZ Solucoes. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface FamilyTableViewCell : UITableViewCell
@property (weak, nonatomic) IBOutlet UILabel *familyLabel;
@property (weak, nonatomic) IBOutlet UILabel *valFam;
@property (weak, nonatomic) IBOutlet UILabel *valPerc;

@end
