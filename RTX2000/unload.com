��PN.HEX .BNP         �#�-
UNLOAD86 ver. 1.0 �� �
�tٵ F�< ��t�</tʍ>��FG�< t</t����uF�$_<Bt<Et	<Pt���_ ������U ������K FI�<0r�<Ar
$_<Gs�,7�<:s�,0P<r���XV�6	��2������ ����^��G� �6��>��< ��FG<.t<0��O� �
�t�6
��6� �=���!��s#�.
Failure to Open Input file ��� �<���!s$��
Failure to Open output file �����
�t�����@� �!s�=�>� ����
�uM��  ���?�!s$�
Error Reading Input file �f
=  u3�
�u&����
�t��- 9ru��H9ru�H�x�
�tK�6�  �>=�BG�Q� ��r�&�%G���	 �&�%G���FYFG�G�
GQV� �b ^Y��,�3۹  3ҡ��u�{ �6�>=�:GC� ��������������  ��� �����F������
�� ��� ����=���@�!s"�
Error Writing Output file �N�P�
�u�� ��X�WQRVS�>=�:GC��X�  ���P���K��F�����>���9�� �����+�
�� ���o�[^ZY_X��� 
Function Complete. �
�t`�����@� �!sc�C��� 

WARNING:  Input data file too large for output size selected.
 ��
�@���@� �!s����>���!s �Y 
Error Closing Output file! �>���!s�/ 
Error Closing Input file! � L�!:00000001FF
^PR&�
�t� F��FZXVô���!�*�P$���������� �CGX$� �CG�<
s0�7�                                                                                                                                                                        ��

Usage:  UNLOAD86 FILENAME.TYP[/OPTIONS]

Where Options are:
     B for BNPF instead of INTeL hex.(Overrides all other options!)
     E to use extended addressing in INTeL hex.
     P to fill output file to end of EPROM.
       Uses the following subcommands:
         0  32 Bytes         1  64 Bytes
         2  128 Bytes        3  256 Bytes
         4  512 Bytes        5  1K Bytes
         6  2K Bytes         7  4K Bytes
         8  8K Bytes         9  16K Bytes
         A  32K Bytes        B  64K Bytes
         C  *128K Bytes      D  *256K Bytes
         E  *512K Bytes      F  *1Meg Bytes

         * Forces extended addressing

INTeL hex output files are .HEX
BNPF output files are .BNP
 �,�     ?      �   �  �  �  �  �  �?  �  �� �� 0�� p�� ����������������������������������������������������