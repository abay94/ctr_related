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
uint8_t chanelRel;
uint8_t valRel;
uint8_t stateRunLed;
uint8_t stateErrorLed;
uint16_t AdcValue;

char adcValStr[2];

//------HANDLE DI -------------------------
int handle_DI(uint8_t channel)
{
    Result = Ctr700DrvGetDigiIn(channel, &state);
    if (Result != kCtr700DrvResult_Success)
    {
        char *error = "Failed to control DI";
        write(STDOUT_FILENO, error, strlen(error));
        return EXIT_FAILURE;
    }
    if (state == (uint8_t)1)
    {
        write(STDOUT_FILENO, "1", strlen("1"));
    }
    if (state == (uint8_t)0)
    {
        write(STDOUT_FILENO, "0", strlen("0"));
    }

    return 0;
}
//------HANDLE DO -------------------------
int handle_DO(uint8_t channelDO, uint8_t valueDO)
{
    Result = Ctr700DrvSetDigiOut(channelDO, valueDO);
    if (Result != kCtr700DrvResult_Success)
    {
        char *error = "Failed to control DO";
        write(STDOUT_FILENO, error, strlen(error));
        return EXIT_FAILURE;
    }
    write(STDOUT_FILENO, "ok", strlen("ok"));

    return 0;
}
//------HANDLE Run led-------------------------
int handle_Run_Led(uint8_t state)
{
    Result = Ctr700DrvSetRunLed(state);
    if (Result != kCtr700DrvResult_Success)
    {
        char *error = "Failed to control RUN led";
        write(STDOUT_FILENO, error, strlen(error));
        return EXIT_FAILURE;
    }
    write(STDOUT_FILENO, "ok", strlen("ok"));

    return 0;
}

//------HANDLE Error led -------------------------
int handle_Err_Led(uint8_t state)
{
    Result = Ctr700DrvSetErrLed(state);
    if (Result != kCtr700DrvResult_Success)
    {
        char *error = "Failed to control ERROR led";
        write(STDOUT_FILENO, error, strlen(error));
        return EXIT_FAILURE;
    }
    write(STDOUT_FILENO, "ok", strlen("ok"));

    return 0;
}
//------HANDLE RELAY -------------------------
int handle_REL(uint8_t channelREL, uint8_t valueREL)
{
    Result = Ctr700DrvSetRelay(channelREL, valueREL);
    if (Result != kCtr700DrvResult_Success)
    {
        char *error = "Failed to control Relay";
        write(STDOUT_FILENO, error, strlen(error));
        return EXIT_FAILURE;
    }
    write(STDOUT_FILENO, "ok", strlen("ok"));

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
    // adcValStr[0] = AdcValue & 0xFF;
    // adcValStr[1] = AdcValue >> 8;
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
    write(STDOUT_FILENO, "OK", strlen("OK"));
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
                    valMode = (uint8_t)atoi(commands[2]);
                    handle_AI(chanelAI, valMode);
                }
                if (strcmp(commands[0], "rel") == 0)
                {
                    chanelRel = (uint8_t)atoi(commands[1]);
                    valRel = (uint8_t)atoi(commands[2]);
                    handle_REL(chanelRel, valRel);
                }
                if (strcmp(commands[0], "ledrun") == 0)
                {
                    stateRunLed = (uint8_t)atoi(commands[1]);
                    handle_Run_Led(stateRunLed);
                }
                if (strcmp(commands[0], "lederr") == 0)
                {
                    stateRunLed = (uint8_t)atoi(commands[1]);
                    handle_Err_Led(stateRunLed);
                }
            }
        }
    }
    return 0;
}