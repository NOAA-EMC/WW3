## WAVEWATCH III Trusted Repos Code Manager Meeting - Minutes - May 20th 2019 0930EDT

### Click [here](https://github.com/NOAA-EMC/WW3/edit/gh-pages/repoadmin/9773878d237ad1f0b31f1354f3b87239c0805fba/Minutes_20190520/index.md) to edit this file 

### Participants:
- Ifremer: Mickael
- NCEP-EMC: Ali, Henrique
- UKMet: Andy
- USACE: Tyler

1. WW3 Development meeting
- Canceled → Reschedule
 + Proposed date: 6/5/19 10:30am
 + Andy Saulter will present

2. Regtests, matrix scripts
- Ali to start document on regtesting, Jessica will add wiki page
  + Status?
  + Ali has started to put something down but not published
  + Tyler and Henrique had an email with procedure that would work as skeleton of page
  + Ali: page will be made available by Friday May 20th
  + Henrique testing usage of mpiserial to speed up shared memory tests in matrix
    * Tests indicated this is not a viable option at this stage, more work needed
- Status of regtests/matrix at trusted repos
  + ERDC has not yet made progress, will work with Ali this week
  + UKMO Chris has been working through it but is not there yet, has made good progress
  + Will report on during next two weeks

3. Transferring branches Vlab-> github 
- Status?
  + Mickael has no branches, Fabrice's branches have been merged 
  + Mickael will confirm with Fabrice for deletion
    * Still waiting to hear from Fabrice  
- Tag up with ERDC to figure out what has been ported or restarted from scratch (then delete branches on vlab): Ali will check
  + This week will have meeting with Ty and Aron and update the Valb → GitHub spreadsheet
- Ali and Jessica shared spreadsheet with branches that need to be transferred.

4. Updating develop and master (next two weeks time frame)
- Develop
  + Update to bugfix fixed file in ounf for NCEP applications
  + Update to bugfix ww3_shel (Andres Sepulveda/Mickael) (Hotfix)
  + Shared object ww3 library (Stelios: pull request)
  + Ifremer: 
    * bugfix for ounf (Hotfix)
    * bigger bugfix for prnc (Hotfix)
- Master
  + Noted Hotfixes for develop will make it to master -> New tag v6.07.2

5. New developments
  + Milestones discussed last meeting were added to GitHub issues
  + Status of Milestones/issues

6. Forum/FAQ
  + Any further ideas on forum?
    * None this time
  + Status of adding FAQ to wiki (Jessica and Mickael)
    * Jessica will report after returning from travel

7. Updates on activities in each trusted repo
- NCEP
  + Started updates to esmf directory in feature branch (Makefile for NEMS applications), using generalized cmplr.env and comp/link.tmpl
    * this will serve as the basis for branch to eliminate specific comp and link scripts
  + No progress on strategy to port gridgen to GitHub
  + No progress on current wiki page and GitFlow inconsistencies
  + Henrique added io page, will further expand to include manual and other docs
    * io page will host minutes that can be edited by all
  + Shared object ww3 library (Stelios: pull request) 
- ERDC
  + fix for triads
  + limiter for shallow water
  + Thornton/Guza + Neumann boundary
  + Regtest for Boers case
  + Some of these features/bugfixes may enter develop in the next two weeks
    * Tyler will report
- UKMO
  + Issue with Cray parallel make. Status?
  + Andy will add his code to uprstr over Summer

8. Unfulfilled to dos:
  + Need to add trusted repos POC addresses to GitHub wiki 
  + Add code manager email to Ifremer (Henrique will add Mickael)


