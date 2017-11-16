using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.IO;
using System;
using System.Text;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;


public class Molrang : MonoBehaviour {

	public string path;
	public string outPath;
	// private string path = "D:/CodeWork/xae";

	IEnumerator zom(){
		yield return new WaitForSeconds(1);
	}
	public void JustString(){
		int		totalTrial;
		int		successTrial;
        char[]	delimiterChars = { ':' };
		bool	isUserid = false;
		string	strLine, writeLine = "";
		int		i=0, totalCount=0, trueCount=0;

		//D:/CodeWork/test3.json file
        TextWriter tw = new StreamWriter(outPath);

		using (FileStream fs = new FileStream(path, FileMode.Open, FileAccess.Read))
		using (StreamReader sr = new StreamReader(fs)){
			while( !sr.EndOfStream ){
				strLine = sr.ReadLine();
				Debug.Log("strLine : " + strLine);
				if( strLine.Contains("userId")){
					//Write file 
					if(i>0){
						Debug.Log("writeLine : " + writeLine);
						writeLine +=  "," + trueCount.ToString() + "," + totalCount.ToString();
						tw.WriteLine(writeLine);
						//reset count
						totalCount = 0;
						trueCount = 0;	
					}else{
						//write head
						tw.WriteLine("userid, truetrialcount, totaltrialcount");
					}

					string[] tokens = strLine.Split(delimiterChars);	
					Debug.Log("					writeLine : " + writeLine);
					if( tokens.Length > 1){
						writeLine = String.Copy(tokens[1]);
						Debug.Log("						UserId	tokens[0], tokens[1] : " + tokens[0] + tokens[1]);
					}
				}else if( strLine.Contains("isSuccessful")){
					totalCount++;	
					string[] tokens = strLine.Split(delimiterChars);	
					Debug.Log("						issuccess	tokens[0], tokens[1] : " + tokens[0] + tokens[1]);					
					if( tokens.Length > 1){
						if( tokens[1].Contains("true") ){
							trueCount++;		
						}
					}
				}
				i++;		
			}
			// final line write
			writeLine +=  "," + trueCount.ToString() + "," + totalCount.ToString();
			tw.WriteLine(writeLine);
			tw.Close();
		}
	}
	public void JustJsonToString(){
		//xaa2.json file
		string	strLine, writeLine = "";
		string	userIdStr, userId;
		bool	isBasicInfo = false;
		bool	isProbeBudget = false;
		bool	isProbes = false;
		bool	isRole = false;
		long	probeBudget = 0;	
        char[] delimiterChars = { ',', ':' };
		int 	i =0;
        TextWriter tw = new StreamWriter(outPath);

		tw.WriteLine("action,note,timestamp,tool,userId");

		using (FileStream fs = new FileStream(path, FileMode.Open, FileAccess.Read))
		using (StreamReader sr = new StreamReader(fs)){
			while( !sr.EndOfStream ){
				strLine = sr.ReadLine();
				Debug.Log("strLine : " + strLine);
				if( strLine.Contains("action") 
					|| strLine.Contains("note")
					|| strLine.Contains("timestamp")
					|| strLine.Contains("tool") 
					|| strLine.Contains("userId") 
										  ){
					string[] tokens = strLine.Split(delimiterChars);
					if( tokens.Length > 1){
						JProperty jp = new JProperty(tokens[0], tokens[1]);
						// Debug.Log("						JProperty	tokens[0], tokens[1] : " + tokens[0] + tokens[1]);
						// Debug.Log("						JProperty name value : " + (string)jp.Name + (string)jp.Value);
						writeLine = writeLine + ((string)jp.Value).Replace("\"","");
						if( ((string)jp.Name).Contains("userId") ) {
							tw.WriteLine(writeLine);
							writeLine = "";
						}else
							writeLine = writeLine + ",";
					}

				}
				i++;
				if(i% 100 == 0)
					tw.Flush();;
			}

		}
		tw.Close();
		
	}


	public void HandleJon(){

		JObject obj;
		JObject obj2;
		int i=0;


		using (FileStream fs = new FileStream(path, FileMode.Open, FileAccess.Read))
		using (StreamReader sr = new StreamReader(fs))
		using (JsonTextReader reader = new JsonTextReader(sr))
		{
			reader.SupportMultipleContent = true;

			while(true){
		        // var message = serializer.Deserialize<string>(reader);

				if( i> 1000)
					break;
				if( !reader.Read() ){
					break;
				}else if( sr.EndOfStream ){
					break;
				}else{
					Debug.Log("i:					" + i);
				}



				if (reader.Value != null){
					Debug.Log("Token & Value 			" +  reader.TokenType + " : "+ reader.Value);
				}
				else{
					Debug.Log("TokenOnly					" + reader.TokenType);
					Debug.Log("TokenOnly			IsDefined		" + JsonToken.IsDefined(typeof(JsonToken), reader.TokenType));
					
					if( !JsonToken.IsDefined(typeof(JsonToken), reader.TokenType)){
						sr.ReadLine();	
						Debug.Log("						ReadLine");
					}else if(reader.TokenType == JsonToken.Undefined){
						sr.ReadLine();	
						Debug.Log("						Undefined, Readline");
					}
					
						
				}

				StartCoroutine(zom());
				// try{
				// 	obj = JObject.Load(reader);
				// 	// obj2 = obj["probes"] as JObject;
				// 	JToken j1 = obj.First;
				// 	var p = j1 as JProperty;
				// 	Debug.Log(p.Name);//key값이다
					// Debug.Log(obj["00Iypjs64pYN8YSK6eqSC5OK3FR2"] );
					// foreach(var item in obj2){
					//     Debug.Log(item.key);//key값이다
					//     Debug.Log(item.Value);//Value값이다
					// }
				// }catch(Exception e){
				// 	// Debug.Log(sr.Read().ToString());
				// 	Debug.Log(e);

				i++;	
			}
			

			// while (reader.Read())
			// {
			// 	if (reader.TokenType == JsonToken.StartObject)
			// 	{
			// 		JObject obj = JObject.Load(reader);
			// 	}
			// }
		}

	}
}
