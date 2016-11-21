//
//  FamilyViewController.m
//  MLApp
//
//  Created by Joao Pacheco on 2/4/15.
//  Copyright (c) 2015 SZ Solucoes. All rights reserved.
//

#import "FamilyViewController.h"
#import "FamilyTableViewCell.h"
#import "ItemsViewController.h"
#import "ApprovalViewController.h"
#import <QuartzCore/QuartzCore.h>

@interface FamilyViewController ()

@end

@implementation FamilyViewController

@synthesize listFamily;
@synthesize docKey;
@synthesize doc;
@synthesize btnReject;
@synthesize btnApproval;

- (id)initWithStyle:(UITableViewStyle)style
{
    self = [super initWithStyle:style];
    if (self) {
        
    }
    return self;
}

- (void)viewDidLoad {
    
    [super viewDidLoad];
    
    self.title = [NSString stringWithFormat:@"%@", self.docKey];
    
    [self.tableView reloadData];
    
    UIBarButtonItem *backItem = [[UIBarButtonItem alloc] initWithTitle:@"Famílias"
                                                                 style:UIBarButtonItemStyleBordered
                                                                target:nil
                                                                action:nil];
    
    [self.navigationItem setBackBarButtonItem:backItem];
    
    //self.btnReject
    
    // Uncomment the following line to preserve selection between presentations.
    // self.clearsSelectionOnViewWillAppear = NO;
    
    // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
    // self.navigationItem.rightBarButtonItem = self.editButtonItem;
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - Table view data source

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    // Return the number of sections.
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    // Return the number of rows in the section.
    return [self.listFamily count];
}


- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    
    static NSString *CellIdentifier = @"familyCell";
    FamilyTableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier forIndexPath:indexPath];
    
    NSDictionary *fam;
    fam = [self.listFamily objectAtIndex:indexPath.row];
    
    NSString *showBtn = [NSString stringWithFormat:@"%@", [self.doc valueForKey:@"showApproval"]];
    
    if ([showBtn boolValue]) {
        
        [btnApproval setEnabled:TRUE];
        [btnApproval setTintColor:[UIColor colorWithRed:(0/255.0) green:(128/255.0) blue:(64/255.0) alpha:1]];
        [btnReject setEnabled:TRUE];
        [btnReject setTintColor:[UIColor colorWithRed:(255/255.0) green:(0/255.0) blue:(0/255.0) alpha:1]];
        
    } else {
        
        [btnApproval setEnabled:FALSE];
        [btnApproval setTintColor:[UIColor colorWithRed:(248/255.0) green:(248/255.0) blue:(248/255.0) alpha:1]];
        [btnReject setEnabled:FALSE];
        [btnReject setTintColor:[UIColor colorWithRed:(248/255.0) green:(248/255.0) blue:(248/255.0) alpha:1]];
        
    }
    
    cell.familyLabel.text = [NSString stringWithFormat:@"%@", [fam valueForKey:@"famDes"]];
    cell.valFam.text = [NSString stringWithFormat:@"Valor: R$ %@",[fam valueForKey: @"famValue"]];
    cell.valPerc.text = [NSString stringWithFormat:@"%@: %@%@", [fam valueForKey:@"typeDes"] ,[fam valueForKey:@"percValue"], @"%"];
    
    return cell;
}

- (void)viewWillAppear:(BOOL)animated
{
    [self.navigationController setToolbarHidden:NO animated:YES];
}

- (void)viewWillDisappear:(BOOL)animated
{
    [self.navigationController setToolbarHidden:YES animated:YES];
}


/*
 // Override to support conditional editing of the table view.
 - (BOOL)tableView:(UITableView *)tableView canEditRowAtIndexPath:(NSIndexPath *)indexPath {
 // Return NO if you do not want the specified item to be editable.
 return YES;
 }
 */

/*
 // Override to support editing the table view.
 - (void)tableView:(UITableView *)tableView commitEditingStyle:(UITableViewCellEditingStyle)editingStyle forRowAtIndexPath:(NSIndexPath *)indexPath {
 if (editingStyle == UITableViewCellEditingStyleDelete) {
 // Delete the row from the data source
 [tableView deleteRowsAtIndexPaths:@[indexPath] withRowAnimation:UITableViewRowAnimationFade];
 } else if (editingStyle == UITableViewCellEditingStyleInsert) {
 // Create a new instance of the appropriate class, insert it into the array, and add a new row to the table view
 }
 }
 */

/*
 // Override to support rearranging the table view.
 - (void)tableView:(UITableView *)tableView moveRowAtIndexPath:(NSIndexPath *)fromIndexPath toIndexPath:(NSIndexPath *)toIndexPath {
 }
 */

/*
 // Override to support conditional rearranging of the table view.
 - (BOOL)tableView:(UITableView *)tableView canMoveRowAtIndexPath:(NSIndexPath *)indexPath {
 // Return NO if you do not want the item to be re-orderable.
 return YES;
 }
 */

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    
    [self performSegueWithIdentifier:@"ShowItems" sender:self];
    
}

- (IBAction)doApproval:(id)sender {
    
    [self performSegueWithIdentifier:@"ShowApprovalFam" sender:self];
    
}

- (IBAction)doReject:(id)sender {
    
    [self performSegueWithIdentifier:@"ShowRejectFam" sender:self];
    
}


#pragma mark - Navigation

// In a storyboard-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    
    if ([[segue identifier] isEqualToString:@"ShowItems"]) {
        
        NSIndexPath *selectedRowIndex = [self.tableView indexPathForSelectedRow];
        
        NSDictionary *family = [self.listFamily objectAtIndex:selectedRowIndex.row];
        
        ItemsViewController *items = [segue destinationViewController];
        items.listOfItems = [family objectForKey:@"items"];
        items.docType = [family objectForKey:@"typeDes"];
        
    }
    
    if ([[segue identifier] isEqualToString:@"ShowApprovalFam"]) {
        
        ApprovalViewController *approval = [segue destinationViewController];
        approval.nrTrans = [self.doc objectForKey:@"id"];
        approval.acao = AcaoAprovar;
        
    }
    
    if ([[segue identifier] isEqualToString:@"ShowRejectFam"]) {
        
        ApprovalViewController *approval = [segue destinationViewController];
        approval.nrTrans = [self.doc objectForKey:@"id"];
        approval.acao = AcaoRejeitar;
        
    }
}

@end
