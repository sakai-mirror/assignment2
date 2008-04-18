


if (typeof XMLHttpRequest === "undefined") 
{
	if (window.ActiveXObject) {
		XMLHttpRequest = function () {
			return new ActiveXObject(navigator.userAgent.indexOf("MSIE 5") >= 0 ?
			"Microsoft.XMLHTTP" : "Msxml2.XMLHTTP");
		};
	} 
	
}

var SData = {
	
	/**
	 * Sends a Ajax Request to the server based on the options passed in.
	 * options.httpMethod is the standard method (string default GET)
	 * options.url is the URL to send to (string required)
	 * options.onFail is the function that is invoked on a failed request
	 * options.onSuccess is the function that is invokec on sucess
	 * options.onComplete is called when the request is complete (fail or success)
	 * options.onTimeout is called when the requests timesout
	 * options.timeout is the timeout in ms (default 30000ms)
	 * options.responseType is the response type (default text)
	 * 		text : just return the text
	 *      xml : return a dom
	 *      script : run the script against the window
	 *      If there is anything that indicates XML in the content type a DOM will be returned.
	 * 
	 * options.sync true of synchronouse, false if async (default false)
	 * options.contentType the content type of the POST, if a post (default text/plain)
	 * options.postData an array of data 
	 * options.getData a fucntion to get the data to be sent in the body.
	 * 
	 * GET,HEAD.DELETE
	 * If the options.httpMethod is a GET,HEAD,DELETE then the url is invoked with no body
	 * 
	 * PUT
	 * If getData is defined, this is used to retrieve the body of the post, if that is not 
	 * set postData is used.
	 * 
	 * POST
	 * If getData is defined, this is used, otherwise, getData is assumed to be an array.
	 * If the form is url encoded, then postData should be a name value array.
	 * If the form is multipart, then the postData should be a name value array, where 
	 * the value of the array is an object
	 * The name of the element in the array is used as the form element name.
	 * value.fileName is used as a filename for the form element
	 * value.contentType is used as the content type for the element
	 * value.data is the data for the element
	 * 
	 * The fucntion will not perform file uplaods from a file input form, for that you should use
	 * SWFUpload and do it all via flash.
	 * 
	 * 
	 * 
	 * 	  
	 * @param options a structure of options
	 */
	ajax : function (options) {
		/**
		 * The options structure with defaults
		 */
		var opt = {
			httpMethod : options.httpMethod || "GET",
			url : options.url || "",
			onFail : options.onFail || function () {},
			onSuccess : options.onSuccess || function () {},
			onComplete : options.onComplete || function () {},
			onTimeout : options.onTimeout || function () {},
			sync : !options.async || false,
			contentType : options.contentType || "text/plain",
			postData : options.postData || null,
			getData : options.getData || null,
			timepout : options.timeout || 30000,
			responseType : options.responseType || "text"			
		};
		/**
		 * is the response Ok
		 */
		function httpOk(response) {
			try {
				return !response.status && location.protocol === "file:" ||
				(response.status >= 200 && response.status < 300) ||
				(response.status === 304) ||
				(navigator.userAgent.indexOf("Safari") >= 0 && 
					typeof response.status === "undefined");
			} catch (e) {
			}
			return false;
		}
		/**
		 * get the response data and if a script, run the script
		 */
		function getResponseData(response, responseType) {
		    var rct = response.getResponseHeader("content-type");
		    var data = !responseType && rct && rct.indexOf("xml") >= 0;
		    data = responseType === "xml" || data ? response.responseXML : response.responseText;
		    if (responseType === "script") {
		 	   eval.call(window, data);
		    } 
		    return data;
		}
		
		var xmlHttp = new XMLHttpRequest();
		xmlHttp.open(opt.httpMethod, opt.url, !opt.sync);
		xmlHttp.setRequestHeader("x-ajax-client", "SData Client 1.0");
		/**
		 * A timeout function
		 */
		var requestTimeout = false;
		setTimeout(function () {
			requestTimeout = true;
			opt.onTimeout();
		}, opt.timeout);
		
		/**
		 * the on ready event
		 */
	    xmlHttp.onreadystatechange = function () {
			if (xmlHttp.readyState === 4 && !requestTimeout) {
				if (httpOk(xmlHttp)) {
			 	   opt.onSuccess(getResponseData(xmlHttp, opt.responseType));
				} else {
				    opt.onFail(xmlHttp.status);
				}
			    opt.onComplete();
			    xmlHttp = null;
			}
	    };
	    
		
		
		/**
		 * get the request body
		 */
		var out = [];
		var outputData = null;
		if (opt.httpMethod === "POST" || opt.httpMethod === "PUT") {
			if (opt.getData !== null) {
				outputData = opt.getData();
			} else if (opt.httpMethod === "POST" || opt.postData !== null) {
				if (opt.contentType === "application/x-www-form-urlencoded") {
					if (opt.postData.constructor === Array) {
						for ( var i = 0; i < opt.postData.length; i++ ) {
							out.push( opt.postData[i].name + "=" + encodeURIComponent(opt.postData[i].value));
						}		
					} else {
						for ( var j in opt.postData ) {
							out.push(j+"="+encodeURIComponent(opt.postData[j]));
						}
					}
					outputData = out.join("&");
				} else if ( opt.contentType === "multipart/form-data" )  {
					if ( opt.postData.constructor === Array ) {
						for ( var k = 0; k < opt.postData.length; k++ ) {
							var name = opt.postData[k].name;
							var value = opt.postData[k].value;
							var fileName = value.fileName || null;
							var contentType = value.contentType || 'text/plain';
							if ( fileName !== null ) {
								fileName = ' filename="' + fileName + '"'; 
							}
							out.push(
								'\r\n'+ 
								'Content-Disposition: form-data; name="' + name + '";' + fileName + '\r\n'+ 
								'Content-Type: '+contentType+ '\r\n' +
								'\r\n'+
								value.data+
								'\r\n');
						}		
					} else {
						for ( var l in opt.postData ) {
							var fileName = opt.postData[l].fileName || null;
							var fileName2 = opt.postData[l].fileName || null;
							var contentType = opt.postData[l].contentType || 'text/plain';
							if ( fileName !== null ) {
								fileName = ' filename="' + fileName + '"'; 
							}
							out.push(
								'\r\n' +
								'Content-Disposition: form-data; name="' + fileName2 + '";' + fileName + '\r\n'+ 
								'Content-Type: '+contentType+ '\r\n' +
								'\r\n'+
								opt.postData[l].data+
								'\r\n');
						}
					}
					var boundaryString = "bound"+Math.floor(Math.random() * 9999999999999);
					var boundary = '--' + boundaryString;
					opt.contentType = opt.contentType +"; boundary=" + boundaryString + "";
					//outputData = out.join(boundary) + "--";

					outputData = boundary + '\r\n' +
								'Content-Disposition: form-data; name="' + fileName2 + '";' + fileName + '\r\n'+ 
								'Content-Type: '+contentType+ '\r\n' +
								'\r\n'+
								opt.postData[l].data+
								'\r\n'+
								boundary + "--";

				} else {
					outputData = opt.postData;
				}
			} else {
				outputData = opt.postData;
			}
		}
		
		/**
		 * set the content type and send the request
		 */
		xmlHttp.setRequestHeader("Content-type",opt.contentType);
		if ( xmlHttp.overrideMimeType ) {
			// Mozilla browsers have problems with content length
			xmlHttp.setRequestHeader("Connection","close");
			
		} else {
			if ( outputData !== null ) {		
	    		xmlHttp.setRequestHeader("Content-length", outputData.length);
			}
		}
		xmlHttp.send(outputData);
	},
	
	
	log : function(msg) {
		var logWindow = document.getElementById('log');
		if ( logWindow ) {
			logWindow.innerHTML += "<br />"+msg;
		}
	},
	
	/**
	 * Templating class, renders the named  template, using TrimPath
	 * @author ieb
	 * @singleton
	 */
	Template : {
		
		/**
		 * A persistant cache for the templates
		 * 
		 */
		templateCache : [],
		
		/**
		 * render the temlate with a context
		 * @param templateName the name of the element ID.
		 * @param contextObject
		 */
		render : function(templateName,contextObject)  {
				if ( ! SData.Template.templateCache[templateName] ) {
					 var templateNode = document.getElementById(templateName);
					 var firstNode = templateNode.firstChild;
					 var template = null;
					 if ( firstNode && ( firstNode.nodeType === 8 || firstNode.nodeType === 4)) {
					 	template = templateNode.firstChild.data.toString();
					 	
					 } else {
					 	template = templateNode.innerHTML.toString();
					 }
					 SData.Template.templateCache[templateName] = TrimPath.parseTemplate(template,templateName);
				}
				return SData.Template.templateCache[templateName].process(contextObject);
		},
		
		/**
		 * test the template and inject it into test div 
		 * @param target the target test div
		 * @param templates an array of template
		 */
		test : function(target,templates) {
				var rc = {
					response : {},
	            	id : 'f' + Math.floor(Math.random() * 999999999)
				};
				for ( var name in templates ) {
					document.getElementById(target).innerHTML += SData.Template.render(templates[name],rc);
				}
		}
	
	},


	Util : {
		formatBytes: function(nbytes) {
			if ( nbytes < 1024 ) {
				return nbytes.toString()+" B";
			}
			nbytes = nbytes/1024;
			if ( nbytes <  1024 ) {				
				return nbytes.toFixed(2)+" KB";
			}
			nbytes = nbytes/1024;
			if ( nbytes <  1024 ) {
				return nbytes.toFixed(2)+" MB";
			}
			nbytes = nbytes/1024;
			return nbytes.toFixed(2)+" GB";			
		},
		
		formatTime: function(t) {
			if ( t < 0 ) {
				t = 1;
			}
			t = Math.ceil(t);
			var s = t%60;
			var sec = s<10?"0"+s.toString():s.toString();
			t = (t-s)/60;
			var m = t%60;
			var min = m<10?"0"+m.toString():m.toString();
			var h = (t-m)/60;
			var hour = h<10?"0"+h.toString():h.toString();
			return hour+":"+min+":"+sec;
		},
		
		replaceAll : function( str, replacements ) {
		    for ( var i = 0; i < replacements.length; i++ ) {
		        var idx = str.indexOf( replacements[i][0] );
		
		        while ( idx > -1 ) {
		            str = str.replace( replacements[i][0], replacements[i][1] ); 
		            idx = str.indexOf( replacements[i][0] );
		        }
		
		    }
    		return str;
		}				
	},
	
	
	/*
	In your widget you can use the following functions to save/get widget preferences
	
		* Save a preference with feedback:	var response = WidgetPreference.save(preferencename:String, preferencontent:String, myCallbackFunction);	
		
			This will warn the function myCallbackFunction, which should look like this:
			
				function myCallbackFunction(success){
					if (success) {
						//Preference saved successfull
						//Do something ...
					} else {
						//Error saving preference
						//Do something ...
					}
				}
		
		* Save a preference without feedback:	var response = WidgetPreference.quicksave(preferencename:String, preferencontent:String);
		
			This will not warn you when saving the preference was successfull or unsuccessfull
		
		* Get the content of a preference:	var response = WidgetPreference.get(preferencename:String, myCallbackFunction);
		
			This will warn the function myCallbackFunction, which should look like this:
			
				function myCallbackFunction(response, exists){
					if (exists) {
						//Preference exists
						//Do something with response ...
					} else {
						//Preference does not exists
						//Do something ...
					}
				}
	*/

	WidgetPreference : {
		get : function(prefname, callback){ 
			var url= "/sdata/p/widgets/" + prefname;
			url = url +"?sid="+Math.random();
			SData.ajax( {
				url : url,
				onSuccess : function(data) {
					callback(data,true);
				},
				onFail : function(status) {
					callback(status,false);
				},
				onTimeout : function(status) {
					callback("timeout",false);
				}
			});
	
	
	
		},
	
		save : function(prefname, prefcontent, callback){
			var cb = callback || function() {}; 
			var url= "/sdata/p/widgets?sid="+Math.random();
			var data = {"items":{"data": prefcontent,"fileName": prefname,"contentType":"text/plain"}};
			//var data = [];
			//data[prefname].data = prefcontent;
			//data[prefname].fileName = prefname;
			//data[prefname].contentType = "text/plain";
			SData.ajax({
				url :url,
				httpMethod : "POST",
				onSuccess : function(data) {
					cb(data,true);
				},
				onFail : function(status) {
					cb(status,false);
				},
				//onTimeout : function(status) {
				//	cb("timeout",false);
				//},
				postData : data,
				contentType : "multipart/form-data"
			});
				
		}
	}
};
	
	
	
