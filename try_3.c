#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <termios.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/select.h>
#include <sys/ioctl.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>

#include "ctr700drv.h"

//static tCtr700DrvHwInfo Ctr700DrvHwInfo_l;
static char commands[10][10];
tCtr700DrvResult Result;
uint8_t valueDO;
uint8_t chanelDO;
uint8_t chanelDI;
uint8_t state;
uint8_t chanelAI;
uint8_t valMode;
uint16_t AdcValue;
char adcValStr[10];

//------HANDLE DI -------------------------
int handle_DI(uint8_t channel)
{
   Ctr700DrvGetDigiIn(channel, &state);
   if (state == (uint8_t)1)
   {
       write(STDOUT_FILENO, "on", strlen("on"));
   }
   if (state == (uint8_t)0)
   {
       write(STDOUT_FILENO, "off", strlen("off"));
   }

   return 0;
}
//------HANDLE DO -------------------------
int handle_DO(uint8_t channelDO, uint8_t valueDO)
{
    Result = Ctr700DrvSetDigiOut(channelDO, valueDO);
    if (Result != kCtr700DrvResult_Success)
    {
        char * error = "Failed to control DO";
        write(STDOUT_FILENO, error, strlen(error));
        return EXIT_FAILURE;
    }

    return 0;
}
//------HANDLE AI -------------------------
int handle_AI(uint8_t channelAI, uint8_t valueMode)
{
    Result = Ctr700DrvAdcSetMode(channelAI, valueMode);
    Ctr700DrvAdcGetValue(channelAI, &AdcValue);
    if (Result != kCtr700DrvResult_Success)
    {
        char *error = "Failed to set mode for AI";
        write(STDOUT_FILENO, error, strlen(error));
        return EXIT_FAILURE;
    }

    sprintf(adcValStr, "%u", AdcValue);
    write(STDOUT_FILENO, adcValStr, strlen(adcValStr));

    return 0;
}

int main()
{
    //------select for erts and serial-------------------------
    fd_set readfs;
    FD_ZERO(&readfs);
    int maxfd;
    maxfd = 1;
    struct timeval Timeout;
    int result = 1;


    Result = Ctr700DrvInitialize();
    if (Result != kCtr700DrvResult_Success)
    {
        write(STDOUT_FILENO, "Failed to initialize application", strlen("Failed to initialize application"));
        return EXIT_FAILURE;
    }


    //----------LOOP-------------------------
    while (result)
    {
        FD_SET(STDIN_FILENO, &readfs);
        //  FD_SET(port, &readfs);
        Timeout.tv_usec = 0;
        Timeout.tv_sec = 2;

        select(maxfd, &readfs, NULL, NULL, &Timeout);
        if (FD_ISSET(STDIN_FILENO, &readfs))
        {
            int result = 1;
            int bytes;
            ioctl(STDIN_FILENO, FIONREAD, &bytes);
            if (bytes == 0)
            {
                return 0;
            }
            char buf[1024];
            int length;
            while (bytes > 0)
            {
                length = (bytes > 1024 ? 1024 : bytes);
                result = (read(STDIN_FILENO, buf, length) != -1);
                if (!result)
                {
                    break;
                }
                if (strlen(buf) == 3)
                {
                    write(STDOUT_FILENO, "len-4", strlen("len-4"));
                }
                //*******************   Just split a buf by delimeter of space *********************
                char *p;
                int cnt = 0;
                p = strtok(buf, " ");
                while (p != NULL)
                {
                    strcpy(commands[cnt], p);
                    p = strtok(NULL, " ");
                    cnt++;
                                }
                //*******************

                bytes = bytes - length;
                // if (strcmp(commands[0], "info") == 0)
                // {
                //     write(STDOUT_FILENO, "doingg--", strlen("doingg--"));
                //     write(STDOUT_FILENO, commands[1], strlen(commands[1]));
                // }
                if (strcmp(commands[0], "di") == 0)
                {
                    chanelDI = (uint8_t)atoi(commands[1]);
                    handle_DI(chanelDI);    
                }
                if (strcmp(commands[0], "do") == 0)
                {
                    chanelDO = (uint8_t)atoi(commands[1]);
                    valueDO = (uint8_t)atoi(commands[2]);
                    handle_DO(chanelDO, valueDO);
                }
                if (strcmp(commands[0], "ai") == 0)
                {
                    chanelAI = (uint8_t)atoi(commands[1]);
                    AdcValue = (uint8_t)atoi(commands[2]);
                    handle_AI(chanelAI, AdcValue);
                }
            }
        }
    }
    return 0;
}