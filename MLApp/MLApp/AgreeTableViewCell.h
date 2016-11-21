//
//  AgreeTableViewCell.h
//  MLApp
//
//  Created by Joao Pacheco on 14/10/16.
//  Copyright © 2016 SZ Solucoes. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface AgreeTableViewCell : UITableViewCell
@property (weak, nonatomic) IBOutlet UILabel *lblClient;
@property (weak, nonatomic) IBOutlet UILabel *lblAgree;
@property (weak, nonatomic) IBOutlet UILabel *lblDtPay;
@property (weak, nonatomic) IBOutlet UILabel *lblRepres;
@property (weak, nonatomic) IBOutlet UILabel *lblValue;
@property (weak, nonatomic) IBOutlet UILabel *lblArea;
@property (weak, nonatomic) IBOutlet UILabel *lblAction;

@end
