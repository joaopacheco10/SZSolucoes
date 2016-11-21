//
//  ApprovalViewController.h
//  MLApp
//
//  Created by SZ Solucoes on 25/04/14.
//  Copyright (c) 2014 SZ Solucoes. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface ApprovalViewController : UIViewController

typedef enum {
    AcaoAprovar,
    AcaoRejeitar
} Acao;

@property (assign,nonatomic) Acao acao;

@property (weak, nonatomic) IBOutlet UITextView *txtRemarks;

@property (assign, nonatomic) NSString *nrTrans;
@property (assign, nonatomic) NSString *docType;
@property (weak, nonatomic) IBOutlet UIButton *btAprovRej;

- (IBAction)btAprovRejTap:(id)sender;
@end
