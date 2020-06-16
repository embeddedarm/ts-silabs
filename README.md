# ts-silabs
Misc software and routines for the TS silabs supervisory microcontroller.  These routines are intended to work equally in uboot, linux userspace, or even DOS.  In `silabs.c` the main function is
```c
int64_t silab_cmd(int argc, char *const argv[])
```

The first argv[0] is not really used by `silab_cmd()` but is there for simplicity in attaching this routine to a userspace
C `main()` function like this:

```c
/* Example usage of silabs.c for realizing a userspace main() entry point */
int main(int argc, char *const argv[]) {
  return silab_cmd(argc, argv);
}
```

The rest of the args represent the commands and subcommands according to a grammar: (as of Jun 15, 2020).

    help                       Print this help
    status                     Print human-readable status
    sleep <N>                  Enter uC sleep mode (low-power) for N millisec
    reboot                     Power cycle and reboot both board and uC
    wdog                       Exits errorlevel 1 if watchdog armed
    wdog expired               Exits errorlevel 1 if last reboot was from wdog
    wdog set <N>               Arms watchdog for N milliseconds
    wdog feed                  Feeds watchdog
    wdog disable               Disables watchdog
    scaps                      Exits errorlevel 1 if supercaps enabled
    scaps enable               Turns on supercaps for this boot
    scaps disable              Turns off supercaps for this boot
    scaps default enable       Sets supercaps to be default enabled on bootup
    scaps default disable      Sets supercaps to be default disabled on bootup
    scaps current <N>          Sets supercaps charging current in N milliamps
    scaps current default <N>  Sets default charging current to N milliamps
    scaps wait                 Blocks until supercaps reach minimum charge
    scaps wait full            Blocks until supercaps reach max charge
    scaps wait pct <N>         Blocks until scaps reach (max-min)*N/100+min
    scaps pct <N>              Exits errorlevel 1 if scaps at N %
    usb                        Exits errorlevel 1 if USB console connected
    flags <N>                  Exits errorlevel 1 if uC flash flag N set
    flags set <N>              Sets uC flash flag N
    flags clear <N>            Clears uC flash flag N
    fan enable                 Turns on fan
    fan disable                Turns off fan
    
For instance, to feed the watchdog using routines from silabs.c in your own application:

```c
static const char *wdog_feed[] = {"silab", "wdog", "feed"};
silab_cmd(3, wdog_feed);
```

The LISP routines are a work-in-progress but represent a LISP translation of the above silabs.c that also does a bit more.  They require some additional quicklisp systems.

In `silabs.c` there is a porting layer consisting of 3 routines, 

```c
int8_t i2c_eeprom_read(uint8_t adr, uint16_t subadr, uint8_t *buf, int len);
int8_8 i2c_eeprom_write(uint8_t adr, uint16_t subadr, uint8_t *buf, int len);
int wait_hook(int); /* sleep for 100ms */
```

