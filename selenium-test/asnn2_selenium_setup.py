#/usr/bin/env python
"""
This script is to update the installation specific urls and data
inside Sakai so that Selenium scripts can be reused.

Replace the new_assn2_page and new_assn2_mainframe variables for
your local Sakai instance on the Assignments2 page so they look
like the current_* variables.

Usage:

./asnn2_selenium_setup.py
   Update the tests for your local machine.

./asnn2_selenium_setup.py forcheckin
   Revert the values back to their original values so you can
   check in new tests.

"""
import sys
import re
import os

current_asnn2_page='/portal/site/usedtools/page/a1d77745-8fbd-49cb-8049-f82df3b482c7'
current_asnn2_mainframe='Main93edc24cx406bx48a8xb960xd7188a219a0f'

# Replace these two variables for your installation
new_asnn2_page='/portal/site/usedtools/page/ASNN2_TOOLPAGE_ID'
new_asnn2_mainframe='MainASNN2_IFRAME_ID'

def main(args):
    if new_asnn2_page == '' or new_asnn2_mainframe == '':
        print '''You need to update the new Sakai information at the
top of this script. Run again after that.'''

    prepare_checkin = False
    if len(args) == 1 and args[0] == 'forcheckin':
        print("Preparing for checkin")
        sys.exit(0)

    sel_files = [file for file in os.listdir('./') if file.endswith('.html')]
    for file_name in sel_files:
        f = open(file_name)
        content = f.read()
        f.close()

        if prepare_checkin == True:
            content = content.replace(new_asnn2_page, current_asnn2_page)
            content = content.replace(new_asnn2_mainframe, current_asnn2_mainframe)
        else:
            content = content.replace(current_asnn2_page, new_asnn2_page)
            content = content.replace(current_asnn2_mainframe, new_asnn2_mainframe)

        fixed_f = open(file_name, 'w')
        fixed_f.write(content)
        fixed_f.close()
        print("Fixed up %s" % (file_name))


if __name__ == "__main__":
    main(sys.argv[1:])
