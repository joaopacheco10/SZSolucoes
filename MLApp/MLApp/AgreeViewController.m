//
//  AgreeViewController.m
//  MLApp
//
//  Created by Joao Pacheco on 17/10/16.
//  Copyright © 2016 SZ Solucoes. All rights reserved.
//

#import "AgreeViewController.h"
#import "ApprovalViewController.h"
#import <QuartzCore/QuartzCore.h>

@interface AgreeViewController ()

@end

@implementation AgreeViewController

@synthesize numAgree;
@synthesize nomCust;
@synthesize dtPayment;
@synthesize perFimAgree;
@synthesize perIniAgree;
@synthesize valAgree;
@synthesize valOrder;
@synthesize valInvest;
@synthesize nomArea;
@synthesize nomCanal;
@synthesize nomRepres;
@synthesize numParc;
@synthesize desAction;
@synthesize desObj;
@synthesize nomCondPag;
@synthesize agree;
@synthesize btAprov;
@synthesize btReprov;

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do any additional setup after loading the view.
    
    NSLog(@"Teste 4: %@", agree);
    
    numAgree.text    = [NSString stringWithFormat:@"%@", [agree valueForKey:@"numAgree"]];
    nomCust.text     = [agree valueForKey:@"customer"];
    dtPayment.text   = [agree valueForKey:@"payDate"];
    perIniAgree.text = [agree valueForKey:@"perIni"];
    perFimAgree.text = [agree valueForKey:@"perFim"];
    valAgree.text    = [NSString stringWithFormat:@"R$ %@", [agree valueForKey:@"valueAgree"]];
    valOrder.text    = [NSString stringWithFormat:@"R$ %@", [agree valueForKey:@"valueBuy"]];
    valInvest.text   = [NSString stringWithFormat:@"R$ %@", [agree valueForKey:@"valueInvest"]];
    nomArea.text     = [agree valueForKey:@"area"];
    nomCanal.text    = [agree valueForKey:@"canal"];
    nomRepres.text   = [agree valueForKey:@"user"];
    numParc.text     = [agree valueForKey:@"numParc"];
    desAction.text   = [agree valueForKey:@"action"];
    desObj.text      = [agree valueForKey:@"objetivo"];
    nomCondPag.text  = [agree valueForKey:@"condPagto"];
    
    //To make the border look very close to a UITextField
    [desObj.layer setBorderColor:[[[UIColor grayColor] colorWithAlphaComponent:0.5] CGColor]];
    [desObj.layer setBorderWidth:2.0];
    
    //The rounded corner part, where you specify your view's corner radius:
    desObj.layer.cornerRadius = 5;
    desObj.clipsToBounds = YES;
    
    desObj.editable = NO;
    
    NSString *showBtn = [NSString stringWithFormat:@"%@", [self.agree valueForKey:@"showApproval"]];
    
    if ([showBtn boolValue]) {
        
        [btAprov setEnabled:TRUE];
        [btAprov setTintColor:[UIColor colorWithRed:(0/255.0) green:(128/255.0) blue:(64/255.0) alpha:1]];
        [btReprov setEnabled:TRUE];
        [btReprov setTintColor:[UIColor colorWithRed:(255/255.0) green:(0/255.0) blue:(0/255.0) alpha:1]];
        
    } else {
        
        [btAprov setEnabled:FALSE];
        [btAprov setTintColor:[UIColor colorWithRed:(248/255.0) green:(248/255.0) blue:(248/255.0) alpha:1]];
        [btReprov setEnabled:FALSE];
        [btReprov setTintColor:[UIColor colorWithRed:(248/255.0) green:(248/255.0) blue:(248/255.0) alpha:1]];
        
    }
    
    
    NSLog(@"Teste 5");
    
    
}

- (void)viewWillAppear:(BOOL)animated
{
    [self.navigationController setToolbarHidden:NO animated:YES];
}

- (void)viewWillDisappear:(BOOL)animated
{
    [self.navigationController setToolbarHidden:YES animated:YES];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

/*
 #pragma mark - Navigation
 
 // In a storyboard-based application, you will often want to do a little preparation before navigation
 - (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
 // Get the new view controller using [segue destinationViewController].
 // Pass the selected object to the new view controller.
 }
 */

- (IBAction)doAprov:(id)sender {
}

- (IBAction)doReprov:(id)sender {
}

// In a storyboard-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    
    NSLog(@"%@", [segue identifier]);
    
    if ([[segue identifier] isEqualToString:@"showAprovAgree"]) {
        
        ApprovalViewController *approval = [segue destinationViewController];
        approval.nrTrans = [self.agree objectForKey:@"numAgree"];
        approval.acao = AcaoAprovar;
        approval.docType = @"Acordo";
        
    }
    
    if ([[segue identifier] isEqualToString:@"showRejectAgree"]) {
        
        ApprovalViewController *approval = [segue destinationViewController];
        approval.nrTrans = [self.agree objectForKey:@"numAgree"];
        approval.acao = AcaoRejeitar;
        approval.docType = @"Acordo";
    }
}

@end
