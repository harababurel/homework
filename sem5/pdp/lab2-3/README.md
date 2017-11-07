## Multithreaded Matrix Multiplication

### Performance

All times below measure a single multiplication of two `2000x2000` matrices.

<table cellspacing="0" cellpadding="0">
    <tbody>
        <tr>
            <td></td>
            <td><samp># threads</samp></td>
            <td><samp>1</samp></td>
            <td><samp>2</samp></td>
            <td><samp>3</samp></td>
            <td><samp>4</samp></td>
            <td><samp>100</samp></td>
            <td><samp>1000</samp></td>
            <td><samp>2000</samp></td>
        </tr>
        <tr>
            <td rowspan="4"><samp>C++</samp></td>
            <td><samp>Manual threads</samp></td>
            <td><samp>40.2s</samp></td>
            <td><samp>20.1s</samp></td>
            <td><samp>19.8s</samp></td>
            <td><samp>19.2s</samp></td>
            <td><samp>17.5s</samp></td>
            <td><samp>15.7s</samp></td>
            <td><samp>18.6s</samp></td>
        </tr>
        <tr>
            <td><samp>Threadpool (queue=10)</samp></td>
            <td><samp>37.4s</samp></td>
            <td><samp>20.3s</samp></td>
            <td><samp>20.7s</samp></td>
            <td><samp>19.6s</samp></td>
            <td><samp>19.1s</samp></td>
            <td><samp>19.2s</samp></td>
            <td><samp>18.9s</samp></td>
        </tr>
        <tr>
            <td><samp>std::async + std::future</samp></td>
            <td><samp>37.6s</samp></td>
            <td><samp>18.9s</samp></td>
            <td><samp>19.3s</samp></td>
            <td><samp>16.6s</samp></td>
            <td><samp>19.4s</samp></td>
            <td><samp>17.2s</samp></td>
            <td><samp>17.1s</samp></td>
        </tr>
        <tr>
            <td><samp>Sequential</samp></td>
            <td><samp>36.8s</samp></td>
            <td colspan="6" align="center"><samp>N/A</samp></td>
        </tr>
        <tr>
            <td><samp>Java</samp></td>
            <td><samp>Threadpool</samp></td>
            <td colspan="3" align="center"><samp>N/A</samp></td>
            <td><samp>289.6s</samp></td>
            <td><samp>245.9s</samp></td>
            <td><samp>292.4s</samp></td>
            <td align="center"><samp>N/A</samp></td>
        </tr>
    </tbody>
</table>

### Hardware

```
Architecture:        x86_64
CPU op-mode(s):      32-bit, 64-bit
Byte Order:          Little Endian
CPU(s):              4
On-line CPU(s) list: 0-3
Thread(s) per core:  2
Core(s) per socket:  2
Socket(s):           1
NUMA node(s):        1
Vendor ID:           GenuineIntel
CPU family:          6
Model:               142
Model name:          Intel(R) Core(TM) i7-7600U CPU @ 2.80GHz
Stepping:            9
CPU MHz:             2900.000
CPU max MHz:         3900.0000
CPU min MHz:         400.0000
BogoMIPS:            5810.00
Virtualization:      VT-x
L1d cache:           32K
L1i cache:           32K
L2 cache:            256K
L3 cache:            4096K
NUMA node0 CPU(s):   0-3
```

