//
//  DetailsViewController.m
//  MLApp
//
//  Created by SZ Solucoes on 15/04/14.
//  Copyright (c) 2014 SZ Solucoes. All rights reserved.
//

#import "DetailsViewController.h"
#import "ItemsViewController.h"
#import "ApprovalViewController.h"
#import <QuartzCore/QuartzCore.h>

@interface DetailsViewController ()

@end

@implementation DetailsViewController

@synthesize doc;
@synthesize family;
@synthesize txtFornec;
@synthesize txtData;
@synthesize txtSolic;
@synthesize txtTotal;
@synthesize txtObs;
@synthesize itemsCell;
@synthesize isAlternative;
@synthesize txtDtVencto;
@synthesize docKey;
@synthesize txtCCusto;
@synthesize txtCodLotac;
@synthesize txtDesLotac;

bool ISAPPROVING;

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    // Do any additional setup after loading the view.
    
    self.title = @"Detalhe";
    
    //[NSString stringWithFormat:@"%@",[doc valueForKey:@"docKey"]];
    
    txtFornec.text = [doc valueForKey:@"supplier"];
    txtData.text = [doc valueForKey:@"docDate"];
    txtSolic.text = [doc valueForKey:@"user"];
    txtTotal.text = [doc valueForKey:@"value"];
    txtObs.text = [doc valueForKey:@"cause"];
    txtDtVencto.text = [doc valueForKey:@"dueDate"];
    docKey.text = [doc valueForKey:@"docKey"];
    txtCCusto.text = [doc valueForKey:@"codCCusto"];
    txtCodLotac.text = [doc valueForKey:@"codLotac"];
    txtDesLotac.text = [doc valueForKey:@"desLotac"];
    
    NSString *altern = [doc valueForKey:@"alternative"];
    
    [isAlternative setOn:[altern isEqualToString:@"Sim"]];
    
    
    //To make the border look very close to a UITextField
    [txtObs.layer setBorderColor:[[[UIColor grayColor] colorWithAlphaComponent:0.5] CGColor]];
    [txtObs.layer setBorderWidth:2.0];
    
    //The rounded corner part, where you specify your view's corner radius:
    txtObs.layer.cornerRadius = 5;
    txtObs.clipsToBounds = YES;
    
    txtObs.editable = NO;
    
    /*if ([[doc objectForKey:@"items"] count] == 0) {
     self.navigationItem.rightBarButtonItem.title = @"";
     self.navigationItem.rightBarButtonItem.enabled = NO;
     } else {
     self.navigationItem.rightBarButtonItem.title = @"Itens";
     self.navigationItem.rightBarButtonItem.enabled = YES;
     }*/
}

- (void)viewWillAppear:(BOOL)animated
{
    [self.navigationController setToolbarHidden:NO animated:YES];
}

- (void)viewWillDisappear:(BOOL)animated
{
    [self.navigationController setToolbarHidden:YES animated:YES];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void) touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
    [[self view] endEditing:YES];
}

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    if ([[segue identifier] isEqualToString:@"ShowItemsDet"]) {
        
        ItemsViewController *items = [segue destinationViewController];
        items.listOfItems = [self.doc objectForKey:@"items" ];
        
    }
    
    if ([[segue identifier] isEqualToString:@"ShowApprovalDet"]) {
        
        ApprovalViewController *approval = [segue destinationViewController];
        approval.nrTrans = [self.doc objectForKey:@"id"];
        approval.acao = AcaoAprovar;
        
    }
    
    if ([[segue identifier] isEqualToString:@"ShowRejectDet"]) {
        
        ApprovalViewController *approval = [segue destinationViewController];
        approval.nrTrans = [self.doc objectForKey:@"id"];
        approval.acao = AcaoRejeitar;
        
    }
}

- (IBAction)itemTap:(id)sender {
    
    
    [self performSegueWithIdentifier:@"ShowItemsDet" sender:self];
    
}
- (IBAction)doApprovalDet:(id)sender {
    
    [self performSegueWithIdentifier:@"ShowApprovalDet" sender:self];
    
}

- (IBAction)doRejectDet:(id)sender {
    
    [self performSegueWithIdentifier:@"ShowRejectDet" sender:self];
    
}

@end
