var issueScraper = require("../issueScrape");
var nock = require('nock');
var assert = require('chai').assert;

describe("getTransitions", function() {
    describe("TS", function() { 
        beforeEach(function() { 
            var transitions = require("./projectTransitions-TS");
            var statuses = require("./statuses.js");
            var boardColumns = require("./boardColumns-TS");
            var nockJira = nock('http://test')
                    .get('/api/2/status')
                    .reply(200, statuses)

                    .get('/agile/1.0/board/290/configuration')
                    .reply(200, boardColumns);

        });
        it("getTransitions", function(done) {
            var bootstrap = { bootstrap: {project: "TS", jiraApi: "http://test", authHeader: "test"}, getBoardId: "290"}
            issueScraper.getTransitions(bootstrap, function(err, projectCategories) {

expectedCategories = [ 'Open', 'Reopened', '3 Amigos In', '3 Amigos Out', 'Implementation In', 'Implementation Out', 'Review In', 'Review Out', 'Test In', 'Test Out', 'Merged', 'Deployed to Test Environment', 'Deployed to Staging Environment', 'Release Validation', 'Resolved', 'Closed' ]

                assert.deepEqual(projectCategories, expectedCategories);

                return done(err);
            });
        });
    });

    describe("PE", function() {
        beforeEach(function() {
            var transitions = require("./projectTransitions-PE");
            var statuses = require("./statuses.js");
            var boardColumns = require("./boardColumns-PE");
            var nockJira = nock('http://test')
                    .get('/agile/1.0/board/357/configuration')
                    .reply(200, boardColumns)

                    .get('/api/2/status')
                    .reply(200, statuses);

        });
        it("getTransitions", function(done) {
            var bootstrap = { bootstrap: {project: "PE", jiraApi: "http://test", authHeader: "test"}, getBoardId: "357"}
            issueScraper.getTransitions(bootstrap, function(err, projectCategories) {

expectedCategories = [ "Backlog", "Selected", "In Progress", "In Review", "Test In", "Merged", "UAT", "Test Out", "Done"]

                assert.deepEqual(projectCategories, expectedCategories);

                return done(err);
            });
        });
    });

});
