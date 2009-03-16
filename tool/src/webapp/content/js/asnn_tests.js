var asnn2test = asnn2test || {};

(function (asnn2test, jQuery) { // Start Scoped Thing

asnn2test.run = function() {
    var AsnnTests = new jqUnit.TestCase("Assignment 2 Tests");
	
    var curuser = "";

    AsnnTests.test("Sample Test 1", function() {
        jqUnit.expect(3);
        jqUnit.assertTrue("one",true);
        jqUnit.assertTrue("two",true);
        jqUnit.assertTrue("three",true);
    });

    AsnnTests.test("Current user should be no one", function () {
        jqUnit.expect(1);
    
        jqUnit.stop(true);

        jQuery.ajax({
            url: "/direct/session/current.json",
            success: function(data,txtstatus) {
                jqUnit.start();
                var session = JSON.parse(data);
                jqUnit.assertNull("User should be null", session['userId']);   
            },
            failure: function() {
                jqUnit.start();
                jqUnit.assertTrue("ajax fail", false);
            }
        });

    });

    AsnnTests.test("Log in as Instructor", function () {
        jqUnit.expect(1);
        //jqUnit.stop(true);

        jQuery.ajax({
            url: "/direct/session/new",
            type: "POST",
            async: false,
            data: {
                '_username': jQuery("#instid").val(),
                '_password': jQuery("#instpw").val()
            },
        });

        jQuery.ajax({
            url: "/direct/session/current.json",
            async: false,
            success: function(data,txtstatus) {
                jqUnit.start();
                var session = JSON.parse(data);
                jqUnit.assertEquals("User should be the instructor", session['userEid'], jQuery("#instid").val());   
            },
            failure: function() {
                jqUnit.start();
                jqUnit.assertTrue("ajax fail", false);
            }
        });
    });

    AsnnTests.test("Create and Update Assignment", function () {
        jQuery.ajax({
            url: "/direct/assignment2/new",
            async: false,
            data: {
                'title': 'A new assignment',
                'contextId': jQuery("#siteid").val(),
            }
            success: function(xmlhttp,data,txtstatus) {

            }
        });        

    }
    
};


})(asnn2test, jQuery); // End Scoped Thing


jQuery(document).ready(function () {
    jQuery("#runtests-button").click(function () {
        //alert("Run tests button");
        asnn2test.run();
    }); 
});
