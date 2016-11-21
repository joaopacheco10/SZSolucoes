//
//  DocumentTableCell.h
//  MLApp
//
//  Created by SZ Solucoes on 05/04/14.
//  Copyright (c) 2014 SZ Solucoes. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface DocumentTableCell : UITableViewCell
@property (weak, nonatomic) IBOutlet UILabel *documentoLabel;
@property (weak, nonatomic) IBOutlet UILabel *valorLabel;
@property (weak, nonatomic) IBOutlet UILabel *origemLabel;
@property (weak, nonatomic) IBOutlet UILabel *dataLabel;
@property (weak, nonatomic) IBOutlet UILabel *fornecLabel;
@property (weak, nonatomic) IBOutlet UILabel *obsLabel;

@end
