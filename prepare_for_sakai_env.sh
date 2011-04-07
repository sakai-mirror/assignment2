#!/bin/sh

getSvn()
{
  echo "getting svn " $1
  svn checkout $1
}

compileInstall()
{
  echo "compiling installing"
  mvn clean install
}

compileDeploy()
{
  echo "compiling deploying"
  mvn clean install sakai:deploy
}

applyPatch()
{
  echo "applying patch " $1
}



while [ "$version" != "1" -a "$version" != "2" -a "$version" != "3" -a "$version" != "4" ] 
do
  clear
  echo
  echo "This script will download (into a directory in assignment2 named temp), compile and install into your"
  echo "local maven repository some dependencies.  It will patch the dependencies' pom.xml files as well as patch"
  echo "assignment2's pom.xml files to work w/ your branch of sakai.  It will ONLY patch files within the assignment2"
  echo "tree (it does NOT touch anything outside of it).  This assignment2 directory MUST be sitting inside"
  echo "your Sakai tree before you try to run this script."
  echo
  echo "(1) - Sakai 2.7.x branch"
  echo "(2) - Sakai 2.8.x branch"
  echo "(3) - Sakai 2.9.x branch/trunk"
  echo 
  echo "(4) - Exit"
  echo
  echo "Please choose Sakai branch to patch the tool for: \c "
  read version
done

case "$version" in
        1)
                echo "Preparing for a Sakai 2.7.x environment...."
                getSvn "https://source.sakaiproject.org/svn/content-review/trunk temp/content-review-2.9.x"
                cd temp/content-review-2.9.x
                compileInstall
                cd ../..
                getSvn "https://source.sakaiproject.org/svn/taggable/trunk temp/taggable-2.9.x"
                cd temp/taggable-2.9.x
                patch -p1 -i ../../patches/taggable_sakai_2_7_x.patch
                compileInstall
                cd ../..
                patch -p1 -i patches/assignment2_sakai_2_7_x.patch
                echo
                echo "Successfully prepared assignment2 for a sakai 2.7.x environment."
                echo "You should be able to build and deploy assignment2 now."
                echo

                break
                ;;
        2)
                echo "Preparing for a Sakai 2.8.x environment...."
                getSvn "https://source.sakaiproject.org/svn/content-review/trunk temp/content-review-2.9.x"
                cd temp/content-review-2.9.x
                compileInstall
                cd ../..
                getSvn "https://source.sakaiproject.org/svn/taggable/trunk temp/taggable-2.9.x"
                cd temp/taggable-2.9.x
                patch -p1 -i ../../patches/taggable_sakai_2_8_x.patch
                compileInstall
                cd ../..
                patch -p1 -i patches/assignment2_sakai_2_8_x.patch
                echo
                echo "Successfully prepared assignment2 for a sakai 2.8.x environment."
                echo "You should be able to build and deploy assignment2 now."
                echo

                break
                ;;
        3)
                echo "Preparing for a Sakai 2.9.x/trunk environment...."
                echo 
                getSvn "https://source.sakaiproject.org/svn/content-review/trunk temp/content-review-2.9.x"
                cd temp/content-review-2.9.x
                compileInstall
                cd ../..
                echo
                echo "Successfully prepared assignment2 for a sakai 2.9.x/trunk environment."
                echo "You should be able to build and deploy assignment2 now."
                echo

                break
                ;;
        4)
                echo "Exit"
                echo 
                
                break
                ;;
        *)
                echo "Invalid entry"
                ;;
esac




