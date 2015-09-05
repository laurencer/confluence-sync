-- | Example server using a heterogeneous struct.

import Network.XmlRpc.Server
import PersonTH

listPeople :: IO [Person]
listPeople = return [
		     Person { name = "Homer Simpson", age = 38, 
			      spouse = Just "Marge Simpson" },
		     Person { name = "Lisa Simpson", age = 8, spouse = Nothing}
		    ]

main = cgiXmlRpcServer [("listPeople", fun listPeople)]
