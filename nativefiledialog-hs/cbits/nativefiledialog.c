#include "nfd.h"

#include <stdio.h>
#include <stdlib.h>


/* this test should compile on all supported platforms */

// For debugging
int openDialogAndLogResult( void )
{
    nfdchar_t *outPath = NULL;
    nfdresult_t result = NFD_OpenDialog( "*", NULL, &outPath );
    // nfdresult_t result = NFD_OpenDialog( "png,jpg;pdf", NULL, &outPath );
    if ( result == NFD_OKAY )
    {
        puts("Success!");
        puts(outPath);
        free(outPath);
    }
    else if ( result == NFD_CANCEL )
    {
        puts("User pressed cancel.");
    }
    else 
    {
        printf("Error: %s\n", NFD_GetError() );
    }

    return 0;
}

// Select file
nfdchar_t* openDialog(nfdchar_t* filterList, nfdchar_t* defaultPath)
{
    nfdchar_t *outPath = NULL;
    nfdresult_t result = NFD_OpenDialog(filterList, defaultPath, &outPath);
    if ( result == NFD_OKAY )
    {
        return outPath;
    }
    else if ( result == NFD_CANCEL )
    {
        // puts("User pressed cancel.");
    }
    else 
    {
        printf("Error: %s\n", NFD_GetError() );
    }

    return "";
}

// Select folder
nfdchar_t* pickFolder( void )
{
    nfdchar_t *outPath = NULL;
    nfdresult_t result = NFD_PickFolder( NULL, &outPath );
    if ( result == NFD_OKAY )
    {
        // puts("Success!");
        return outPath;
    }
    else if ( result == NFD_CANCEL )
    {
        // puts("User pressed cancel.");
    }
    else 
    {
        printf("Error: %s\n", NFD_GetError() );
    }

    return "";
}

// Save file
nfdchar_t* saveDialog(nfdchar_t* filterList)
{
    nfdchar_t *savePath = NULL;
    nfdresult_t result = NFD_SaveDialog(filterList, NULL, &savePath );
    if ( result == NFD_OKAY )
    {
        // puts("Success!");
        // puts(savePath);
        // free(savePath);
        return savePath;
    }
    else if ( result == NFD_CANCEL )
    {
        // puts("User pressed cancel.");
    }
    else 
    {
        printf("Error: %s\n", NFD_GetError() );
    }

    return "";
}