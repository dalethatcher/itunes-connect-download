*NOTE: This no longer works, please have a look at http://www.apple.com/itunesnews/docs/AppStoreReportingInstructions.pdf for Apple's own 'auto-injest' tool.*

# itunes-connect-download

A screen scraper to download financial reports from iTunes connect.

## Usage

1) Download jar or build from source with 'lein uberjar'.

2) Create a properties file at ~/.itunes-download.properties with:

	username=<itunes username>
	password=<itunes password>
	reports=<directory to store reports>

3) Run with:

	java -jar itunes-connect-download-1.0.0-SNAPSHOT-standalone.jar

## Output

If everything is going ok you should see the output similar to:

$ java -jar itunes-connect-download-1.0.0-SNAPSHOT-standalone.jar
Loading properties from '/Users/dale/.itunes-download.properties'.
Logging in as user 'someuser@somedomain'.
Fetching Sep-2010-Americas.txt.gz.
Fetching Sep-2010-United Kingdom.txt.gz.
Skipping Aug-2010-Japan.txt.gz.
Skipping Jul-2010-Rest of World.txt.gz.
Skipping Jul-2010-Canada.txt.gz.
Skipping Aug-2010-Euro-Zone.txt.gz.
Skipping Jul-2010-Australia.txt.gz.
$

## License

itunes-connect-download is licensed under the GNU Affero General Public License (AGPL) version 3. This license agreement can be found in the LICENSE file.

## Copyright

itunes-connect-download is copyright Dale Thatcher 2010.
All rights reserved.

## Feedback

I hope you find this script useful.  Please let me know if you have any comments or suggestions by contact me via github at http://github.com/dalethatcher/.
