//
//  ItemTableViewCell.h
//  MLApp
//
//  Created by SZ Solucoes on 16/04/14.
//  Copyright (c) 2014 SZ Solucoes. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface ItemTableViewCell : UITableViewCell
@property (weak, nonatomic) IBOutlet UILabel *lblDesc;
@property (weak, nonatomic) IBOutlet UILabel *lblItemCode;
@property (weak, nonatomic) IBOutlet UILabel *lblQuantity;
@property (weak, nonatomic) IBOutlet UILabel *lblValue;
@property (weak, nonatomic) IBOutlet UILabel *lblType;
@property (weak, nonatomic) IBOutlet UILabel *lblValPerc;
@property (weak, nonatomic) IBOutlet UILabel *lblTotal;


@end
