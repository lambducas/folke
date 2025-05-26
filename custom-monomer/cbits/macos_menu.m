#include <AppKit/AppKit.h>

@interface AppDelegate : NSObject
- (void) menuAction: (id)sender;
- (void) buildMenu;
@end

@implementation AppDelegate

- (void) menuAction: (id)sender {
 NSBeep();
 NSLog(@"%@", sender);
}

- (void) buildMenu {
NSMenu *menubar = [NSMenu new];
NSMenuItem *menuBarItem = [NSMenuItem new];
[menubar addItem:menuBarItem];
[NSApp setMainMenu:menubar];
NSMenu *appMenu = [NSMenu new];
NSMenuItem *quitMenuItem = [[NSMenuItem alloc] initWithTitle:@"Quit"
action:@selector(terminate:) keyEquivalent:@"q"];
[appMenu addItem:quitMenuItem];
[menuBarItem setSubmenu:appMenu];

// **** Asst Menu **** //
NSMenuItem *asstMenuItem = [menubar addItemWithTitle:@"" action:nil keyEquivalent:@""];
NSMenu *asstMenu = [[NSMenu alloc] initWithTitle:@"assistant.menu.title"];
[menubar setSubmenu: asstMenu forItem:asstMenuItem];
NSArray *itemArray = @[@"preventRevokeItem", @"autoAuthItem", @"groupMgrMenu", @"newWeChatItem", @"forwardAndReplyItem"];
[asstMenu addItemWithTitle: itemArray[0] action:@selector(menuAction:) keyEquivalent:@""];
[asstMenu addItemWithTitle: itemArray[1] action:@selector(menuAction:) keyEquivalent:@""];
[asstMenu addItemWithTitle: itemArray[2] action:@selector(menuAction:) keyEquivalent:@""];
[asstMenu addItemWithTitle: itemArray[3] action:@selector(menuAction:) keyEquivalent:@""];
[asstMenu addItemWithTitle: itemArray[4] action:@selector(menuAction:) keyEquivalent:@""];
}
@end

int CreateMenu () {
  // NSApplication *application = [NSApplication sharedApplication];
  AppDelegate *appDelegate = [[AppDelegate alloc] init];
  [appDelegate buildMenu];
  return 0;
}