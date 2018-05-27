import requests
import untangle
import json
import re
from bs4 import BeautifulSoup
from urllib.parse import urlparse
from xml.sax.saxutils import escape
import os.path
import configparser
import argparse

# import logging
# try: # for Python 3
#     from http.client import HTTPConnection
# except ImportError:
#     from httplib import HTTPConnection
# HTTPConnection.debuglevel = 1

CA_BUNDLE = '/etc/ssl/ca-bundle.pem'
API_VERSION = '40.0'


class RestApi(object):
    def __init__(self, username, password):
        self.username = username
        self.password = password
        resp = requests.post(
            'https://login.salesforce.com/services/Soap/u/' + API_VERSION,
            data=self.loginMessage(),
            headers={'Content-Type': 'text/xml', 'SOAPAction': 'login'},
            verify=CA_BUNDLE
        )
        self.login = untangle.parse(resp.text).soapenv_Envelope.soapenv_Body.loginResponse.result
        parsed_uri = urlparse(self.login.serverUrl.cdata)
        self.instance = '{uri.scheme}://{uri.netloc}/'.format(uri=parsed_uri)
        self.session_id = self.login.sessionId.cdata

    def loginMessage(self):
        message = "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">" + \
                  "<SOAP-ENV:Header/><SOAP-ENV:Body><login xmlns=\"urn:partner.soap.sforce.com\"><username>" + \
                  escape(self.username) + "</username><password>" + escape(self.password) + \
                  "</password></login></SOAP-ENV:Body></SOAP-ENV:Envelope>"
        return message

    def get(self, path, params):
        url = self.instance + path
        resp = requests.get(url, headers={
            'Authorization': 'Bearer ' + self.session_id,
            'Content-Type': 'application/json',
            'Accept': 'application/json'
        },
            verify=CA_BUNDLE,
            params=params
        )
        return json.loads(resp.text)

    def getApiApex(self):
        return self.get('services/data/v' + API_VERSION + '/tooling/completions', {'type': 'apex'})

    def getSObjectInfo(self):
        return self.get('services/data/v' + API_VERSION + '/sobjects/', {})

    def getDescribe(self, name):
        return self.get('services/data/v' + API_VERSION + '/sobjects/' + name + '/describe/', {})

    def execApexScript(self, code):
        return self.get('services/data/v' + API_VERSION + '/tooling/executeAnonymous', {'anonymousBody': code})


class ApiInfoWriter(object):
    def __init__(self, api):
        self.api = api

    def getMethodMap(self, cls_methods):
        methods = {}
        for method in cls_methods:
            method_name = method['name']
            methods_same_name = []
            if method_name.lower() in methods:
                methods_same_name = methods[method_name.lower()]
            methods_same_name.append(method)
            methods[method_name.lower()] = methods_same_name
        return methods

    def writeApiApex(self, filename):
        with open(filename, 'w') as out:
            api_apex = {}
            for ns_name, ns_classes in self.api.getApiApex()['publicDeclarations'].items():
                ns = {}
                ns['name'] = ns_name
                classes = {}
                for cls_name, cls in ns_classes.items():
                    cls['methods'] = self.getMethodMap(cls['methods'])
                    cls['name'] = cls_name
                    classes[cls_name.lower()] = cls
                ns['classes'] = classes
                api_apex[ns_name.lower()] = ns
            out.write(json.dumps(api_apex, indent=4))

    def writeSObjectDescribe(self, filename):
        with open(filename, 'w') as out:
            describe_all = {}
            for sobject in self.api.getSObjectInfo()['sobjects']:
                if sobject['queryable'] and not sobject['custom']:
                    if '__' not in sobject['name']:
                        sobject_describe = self.api.getDescribe(sobject['name'])
                        print(sobject_describe['name'])
                        fields = {}
                        for field in sobject_describe['fields']:
                            if not field['custom']:
                                fields[field['name'].lower()] = field
                                print('  ' + field['name'] + '(' + field['type'] + ')')
                        sobject_describe['fields'] = fields
                        describe_all[sobject_describe['name'].lower()] = sobject_describe
            out.write(json.dumps(describe_all, indent=4))

    def tryConstructType(self, ns, name):
        code = "Type t = Type.forName('" + ns + "', '" + name + "');\n" + \
               "if (null == t) { t = Type.forName('" + name + "'); }\n" + \
               "try {\n" + \
               "    Object o = t.newInstance();\n" + \
               " } catch (Exception e) {\n" + \
               "    Object o2 = JSON.deserialize('{}', t);    \n" + \
               " }\n"
        result = self.api.execApexScript(code)
        if result is None:
            return False  # internal salesforce error
        else:
            return result['exceptionMessage'] is None

    def checkDefaultCtors(self, api_apex_json, visible_json):
        a_f = open(api_apex_json)
        api_apex = json.loads(a_f.read())
        a_f.close()
        v_f = open(visible_json)
        visible = json.loads(v_f.read())
        v_f.close()
        for ns, classes in visible.items():
            print(ns)
            for className in classes:
                print("  " + api_apex[ns]['classes'][className]['name'])
                api_apex[ns]['classes'][className]['reflection'] = True
                api_apex[ns]['classes'][className]['defaultConstructible'] = self.tryConstructType(ns, className)
        with open(api_apex_json, 'w') as out:
            out.write(json.dumps(api_apex, indent=4))

    def isInterface(self, cls):
        code = "public class Test57839 implements " + cls + "{}"
        result = self.api.execApexScript(code)
        if result is None:
            return False  # internal salesforce error
        else:
            print(result['compileProblem'])
            return result['compileProblem'] is None or \
                "parameterized type" in result['compileProblem'] or \
                "implement" in result['compileProblem']

    def findInterfaces(self, api_apex_json):
        a_f = open(api_apex_json)
        api_apex = json.loads(a_f.read())
        a_f.close()
        for ns, nsData in api_apex.items():
            print(ns)
            for clsKey, clsData in nsData['classes'].items():
                if not api_apex[ns]['classes'][clsKey].get('isInterface', False):
                    print(ns + "." + clsKey)
                    api_apex[ns]['classes'][clsKey]['isInterface'] = self.isInterface(ns + "." + clsKey)
        with open(api_apex_json, 'w') as out:
            out.write(json.dumps(api_apex, indent=4))

    def trimType(self, qname):
        q = qname.split('.', 1)
        if 2 == len(q) and "system" == q[0].lower():
            return q[1].lower()
        return qname

    def validateSuper(self, derived, base):
        code = derived + " a; Boolean r = (a instanceof " + base + "); System.assert(r);"
        result = self.api.execApexScript(code)
        if result is None:
            return False  # internal salesforce error
        elif result['compileProblem'] and "Operation instanceof is always true" in result['compileProblem']:
            return True
        elif result['compileProblem'] and "Operation instanceof is always false" in result['compileProblem']:
            return False
        elif result['compileProblem'] and "Type is not visible" in result['compileProblem']:  # we assume this holds
            return True
        elif result['success']:
            return True
        return False

    def addSuper(self, api_apex_json, info_file):
        a_f = open(api_apex_json)
        api_apex = json.loads(a_f.read())
        a_f.close()
        with open(info_file, 'r') as info:
            for line in info:
                s = line.strip().split(' : ', 1)
                if 2 == len(s):
                    nsKey = s[0].split('.', 1)[0]
                    clsKey = s[0].split('.', 1)[1]
                    # Note: Decimal is kinda an exception with a circular inheritance: Decimal <-> Double
                    if "decimal" != clsKey and self.validateSuper(self.trimType(s[0]), self.trimType(s[1])):
                        api_apex[nsKey]['classes'][clsKey]['super'] = s[1].lower().split('.', 1)
                    else:
                        api_apex[nsKey]['classes'][clsKey]['super'] = None
                        print('invalid:  ' + line)
        with open(api_apex_json, 'w') as out:
            out.write(json.dumps(api_apex, indent=4))

    def getPropertyType(self, cls, prop):
        propertyName = prop['name']
        code = 'public class TestType9485 {} ' + cls + ' a;  TestType9485 b = a.' + propertyName + ';'
        result = self.api.execApexScript(code)
        if result is None:
            return  # internal salesforce error
        else:
            problem = result['compileProblem']
            isStatic = False
            if problem == 'Class static variable cannot be accessed via object instance' or problem.startswith('Static field cannot be referenced from a non static context:'):
                codeStatic = 'public class TestType9485 {} TestType9485 b = ' + cls + '.' + propertyName + ';'
                result = self.api.execApexScript(codeStatic)
                if result is None:
                    return  # internal salesforce error
                else:
                    problem = result['compileProblem']
                    isStatic = True
            matchObj = re.match(r'Illegal assignment from (.*) to TestType9485', problem, re.M | re.I)
            if matchObj:
                print(cls + '-->' + propertyName + ': ' + matchObj.group(1))
                prop['type'] = matchObj.group(1)
                prop['static'] = isStatic
                return
            else:
                print(cls + '-->' + propertyName + ': ' + problem)
        return

    def properties(self, api_apex_json):
        a_f = open(api_apex_json)
        api_apex = json.loads(a_f.read())
        a_f.close()
        for ns, nsData in api_apex.items():
            for clsKey, clsData in nsData['classes'].items():
                methods = api_apex[ns]['classes'][clsKey].get('methods', {})
                if not all(test in methods for test in ('hashcode', 'ordinal', 'values', 'equals')):
                    for prop in api_apex[ns]['classes'][clsKey]['properties']:
                        clsName = clsKey if 'system' == ns else ns + "." + clsKey
                        self.getPropertyType(clsName, prop)
        with open(api_apex_json, 'w') as out:
            out.write(json.dumps(api_apex, indent=4))

    def genApexTypesClass(self, api_apex_json):
        with open(api_apex_json, 'r') as inp:
            api_apex = json.loads(inp.read())
        clsBody = """

    public Type t;

    public TypeWrapper(Type t) {
        this.t = t;
    }

    private static Boolean getInstanceOf(Type t1, Type t2) {
        List<Object> lst;

        try {
            lst = (List<Object>)Type.forName('List<List<' + t2.getName() + '>>').newInstance();
        } catch (Exception e) {
            return false;
        }

        try {
            lst.add(
                Type.forName('List<' + t1.getName() + '>').newInstance()
            );
            return true;
        } catch (TypeException e) {}

        return false;
    }

    public Integer compareTo(Object compareTo) {
        Type t2 = ((TypeWrapper)compareTo).t;

        if (t.equals(t2)) {
            return 0;
        }

        if (getInstanceOf(t2, t)) {
            return 1;
        }

        return -1;
    }

    public static TypeWrapper get(String ns, String name) {
        Type t;

        try {
            if ('System' == ns) {
                t = Type.forName(name);
            } else {
                t = Type.forName(ns, name);
            }
        } catch (Exception e) {}

        if (null != t) {
            return new TypeWrapper(t);
        }

        return null;
    }

    public static TypeWrapper getSuper(String ns1, String name1) {
        List<TypeWrapper> superClasses = new List<TypeWrapper>{};

        TypeWrapper w = get(ns1, name1);

        if (null == w) {
            return null;
        }

        for (String ns : allTypes.keySet()) {
            for (String name : allTypes.get(ns)) {

                if (ns1 == ns && name1 == name) {
                    continue;
                }

                TypeWrapper w2 = get(ns, name);

                if (null == w2) {
                    continue;
                }

                if (getInstanceOf(w.t, w2.t)) {
                    superClasses.add(w2);
                }
            }
        }

        superClasses.sort();

        if (!superClasses.isEmpty()) {
            return superClasses[0];
        }

        if (getInstanceOf(w.t, Object.class)) {
            return new TypeWrapper(Object.class);
        }

        return null;
    }

    public class Batch implements System.Queueable {

        Integer start = 0;
        Integer i = 0;

        String result = '';

        public Batch() {}

        public Batch(Integer start, String result) {
            this.start = start;
            this.result = result;
        }

        public virtual void execute(QueueableContext context) {
            Boolean stop = false;

            for (String ns : TypeWrapper.allTypes.keySet()) {
                for (String name : TypeWrapper.allTypes.get(ns)) {
                    i++;

                    if (i < start+1) {
                        continue;
                    }

                    TypeWrapper w = getSuper(ns, name);

                    if (null == w) {
                        continue;
                    }

                    if ('Object' != w.t.getName()) {
                        result += ns + '.' + name + ' : ' + w.t.getName() + '\\n';
                    }

                    if (0.6 < Limits.getCpuTime()/Limits.getLimitCpuTime()) {
                        stop = true;
                        break;
                    }

                }
                if (stop) {
                    break;
                }
            }

            if (stop) {
                Batch bt = new Batch(i, result);
                System.enqueueJob(bt);
                return;
            }

            Document doc = new Document(
                Body = Blob.valueOf(
                    String.valueOf(i) + '\\n' + result
                ),
                ContentType = 'text',
                Name = 'Parents',
                Type = 'txt',
                FolderId = UserInfo.getUserId()
            );
            insert doc;
        }

    }
}
"""
        mainString = "public class TypeWrapper implements Comparable {\n\n"
        mainString += "    static final Map<String, List<String>> allTypes = new Map<String, List<String>>{\n"
        nsList = []
        for nsKey, nsData in api_apex.items():
            nsString = "        '" + nsKey + "' => new List<String>{\n" + \
                       "            "
            clsNameList = []
            for clsKey, clsData in nsData['classes'].items():
                if not clsData['isInterface']:  # interfaces
                    if not set(('hashcode', 'values', 'ordinal', 'equals')) <= clsData['methods'].keys():  # enums
                        if clsKey not in set(('set', 'list', 'map')):  # collections
                            clsNameList.append("'" + clsKey + "'")
            nsString += str.join(', ', clsNameList) + "\n"
            nsString += '        }'
            nsList.append(nsString)
        mainString += str.join(',\n', nsList) + "\n"
        mainString += "    };\n\n"
        mainString += clsBody
        with open('TypeWrapper.cls-snippet', 'w') as out:
            out.write(mainString)


class ExtendApiFromDocumentation(object):
    def hasClassMethods(self, tag):
        return tag.has_attr('class') and tag.has_attr('id') and tag['id'].lower().endswith('_methods')

    def hasClassProperties(self, tag):
        return tag.has_attr('class') and tag.has_attr('id') and tag['id'].lower().endswith('_properties')

    def hasClassCtors(self, tag):
        return tag.has_attr('class') and tag.has_attr('id') and tag['id'].lower().endswith('_constructors')

    def signatureSection(self, tag):
        return 'div' == tag.name and tag.has_attr('class') and ['section'] == tag['class'] and ('Signature' == tag.h4.string.strip() or 'Syntax' == tag.h4.string.strip())

    def getMethodSignatures(self, html):
        soup = BeautifulSoup(html, 'html.parser')
        methods = soup.find(self.hasClassMethods)
        if not methods:
            return []
        strSignatures = []
        for method in methods.find_all('div', class_="topic reference nested2"):
            signature = method.find(self.signatureSection)
            sig = ''
            for el in signature.find(class_='codeph apex_code').contents:
                sig += el.string
            strSignatures.append(' '.join(sig.split()))
        return strSignatures

    def getEnumSignatures(self, html):
        soup = BeautifulSoup(html, 'html.parser')
        enums = soup.find_all('td', attrs={"data-title": "Value", "class": "entry"})
        if not enums:
            return []
        enumStrings = []
        for enum in enums:
            sig = ''
            for el in enum.find(class_='codeph apex_code').contents:
                sig += el.string
                enumStrings.append('enum ' + ' '.join(sig.split()) + ' {get;}')
        return enumStrings

    def getCtorSignatures(self, html):
        soup = BeautifulSoup(html, 'html.parser')
        methods = soup.find(self.hasClassCtors)
        if not methods:
            return []
        strSignatures = []
        for method in methods.find_all('div', class_="topic reference nested2"):
            signature = method.find(self.signatureSection)
            sig = ''
            for el in signature.find(class_='codeph apex_code').contents:
                sig += el.string
            strSignatures.append(' '.join(sig.split()))
        return strSignatures

    def getPropertySignatures(self, html):
        soup = BeautifulSoup(html, 'html.parser')
        methods = soup.find(self.hasClassProperties)
        if not methods:
            return []
        strSignatures = []
        for method in methods.find_all('div', class_="topic reference nested2"):
            signature = method.find(self.signatureSection)
            sig = ''
            for el in signature.find(class_='codeph apex_code').contents:
                sig += el.string
            strSignatures.append(' '.join(sig.split()))
        return strSignatures

    # load
    def getNode(self, id_, node):
        for c in node['children']:
            if id_ == c['id']:
                return c

    def setClass(self, namespace, currentName, c, result, typ):
        if 1 < len(currentName.split('.')) and namespace == currentName.split('.')[0]:
            currentName = currentName.split('.')[1]
        # prepare missing values
        if not result.get(namespace.lower(), False):
            result[namespace.lower()] = {'name': namespace, typ: {}}
        if not result[namespace.lower()].get(typ):
            result[namespace.lower()][typ] = {}
        if not result[namespace.lower()][typ].get(currentName.lower(), False):
            result[namespace.lower()][typ][currentName.lower()] = {'name': currentName, 'hrefs': set()}
        result[namespace.lower()][typ][currentName.lower()]['hrefs'].add(c['a_attr']['href'].split('#')[0])

    def getClassLinks(self, namespace, name, node, result):
        for c in node['children']:
            currentName = None
            if c.get('text', None) and 1 < len(c.get('text').split()) and (c.get('text').split()[-1] in ['Interface', 'Class', 'Enum']):
                currentName = ''.join([c for c in c.get('text').split()[0] if 0 < ord(c) < 127])
            if currentName:
                if 'Class' == c.get('text').split()[-1]:
                    self.setClass(namespace, currentName, c, result, 'classes')
                elif 'Interface' == c.get('text').split()[-1]:
                    self.setClass(namespace, currentName, c, result, 'interfaces')
                elif 'Enum' == c.get('text').split()[-1]:
                    self.setClass(namespace, currentName, c, result, 'enums')
                name = currentName
            if c.get('children', None):
                self.getClassLinks(namespace, name, c, result)

    def tocLinks(self, m):
        toc = m['toc'][0]
        referenceNode = self.getNode('apex_reference', toc)
        nsNodes = list(filter(lambda n: 1 < len(n['text'].split()) and 'Namespace' == n['text'].split()[1], referenceNode['children']))
        result = {}
        for node in nsNodes:
            namespace = re.sub(r"\s+", "", node['text'].split()[0], flags=re.UNICODE)
            self.getClassLinks(namespace, None, node, result)
        return result

    def fetchPage(self, root, page):
        headers = {
            'Content-Type': 'application/json',
            'Accept': 'application/json'
        }
        url = 'https://developer.salesforce.com/docs/get_document_content/' + root + '/' + page + '/en-us/208.0'
        response = requests.get(url, headers=headers, verify=CA_BUNDLE, params={})
        return response.json().get('content')

    def retrieveSignatures(self, name, typ, pages):
        api_class = {'name': name, 'methods': [], 'properties': [], 'constructors': [], 'isInterface': 'interface' == typ}
        for p in pages:
            cp = self.fetchPage('apexcode', p)
            if 'enum' == typ:
                # enum methods
                api_class['methods'].append('public Boolean equals(Object obj)')
                api_class['methods'].append('public Integer hashCode()')
                api_class['methods'].append('public Integer ordinal()')
                api_class['methods'].append('public List<' + name + '> values()')
                for s in self.getEnumSignatures(cp):
                    api_class['properties'].append(s)
            else:
                for s in self.getCtorSignatures(cp):
                    api_class['constructors'].append(s)
                for s in self.getMethodSignatures(cp):
                    api_class['methods'].append(s)
                for s in self.getPropertySignatures(cp):
                    api_class['properties'].append(s)
        return api_class

    # Apex class reference
    def load(self):
        headers = {
            'Content-Type': 'application/json',
            'Accept': 'application/json'
        }
        url = 'https://developer.salesforce.com/docs/get_document/atlas.en-us.apexcode.meta/en-us/208.0'
        m = requests.get(url, headers=headers, verify=CA_BUNDLE, params={}).json()
        namespaceLinks = self.tocLinks(m)
        self.api_apex_doc = {}
        for (namespace, nsval) in namespaceLinks.items():
            print(namespace)
            api_namespace = {'name': nsval['name'], 'classes': {}}
            self.api_apex_doc[namespace] = api_namespace
            if nsval.get('classes', False):
                for (cname, cval) in nsval.get('classes').items():
                    api_namespace['classes'][cname] = self.retrieveSignatures(cval['name'], 'class', cval['hrefs'])
            if nsval.get('interfaces', False):
                for (iname, ival) in nsval.get('interfaces').items():
                    api_namespace['classes'][iname] = self.retrieveSignatures(ival['name'], 'interface', ival['hrefs'])
            if nsval.get('enums', False):
                for (iname, ival) in nsval.get('enums').items():
                    api_namespace['classes'][iname] = self.retrieveSignatures(ival['name'], 'enum', ival['hrefs'])

    def retrieveComponents(self, p):
        html = self.fetchPage('pages', p)
        soup = BeautifulSoup(html, 'html.parser')
        componentKey = soup.find('span', class_='titlecodeph').string.split(':')
        compNs = componentKey[0][0].upper() + componentKey[0][1:]
        compName = componentKey[1][0].upper() + componentKey[1][1:]
        compCls = compNs + '.' + compName
        print(compCls)
        rows = soup.find_all('tr')
        properties = []
        for row in rows:
            name = row.find('td', attrs={"data-title": "Attribute Name", "class": "entry"})
            typ = row.find('td', attrs={"data-title": "Attribute Type", "class": "entry"})
            if name and typ:
                nameStr = ''
                for el in name.contents:
                    nameStr += el.string
                nameStr = ' '.join(nameStr.split())
                typStr = ''
                for el in typ.contents:
                    typStr += el.string
                typStr = ' '.join(typStr.split())
                properties.append('public ' + typStr + ' ' + nameStr + ' {get; set;}')
        api_class = {'name': compCls, 'methods': [], 'properties': properties, 'constructors': [], 'isInterface': False}
        if not self.api_apex_doc.get('component', False):
            self.api_apex_doc['component'] = {'name': 'Component', 'classes': {}}
        if not self.api_apex_doc['component']['classes'].get(compNs.lower(), False):
            self.api_apex_doc['component']['classes'][compNs.lower()] = {'name': compNs, 'methods': [], 'properties': [], 'constructors': [], 'isInterface': False}
        self.api_apex_doc['component']['classes'][compCls.lower()] = api_class

    # Visualforce reference (components)
    def load2(self):
        headers = {
            'Content-Type': 'application/json',
            'Accept': 'application/json'
        }
        url = 'https://developer.salesforce.com/docs/get_document/atlas.en-us.pages.meta/en-us/208.0'
        m = requests.get(url, headers=headers, verify=CA_BUNDLE, params={}).json()
        compRef = m['toc'][-3]['children']
        print('----- Components -----')
        for lnk in compRef:
            self.retrieveComponents(lnk['a_attr']['href'].split('#')[0])

    def methodSignature(self, isStatic, name, returnType, args):
        return {
            "argTypes": [typ for (typ, name) in args],
            "isStatic": isStatic,
            "methodDoc": None,
            "name": name,
            "parameters": [{"name": name, "type": typ} for (typ, name) in args],
            "references": [],
            "returnType": returnType
        }

    def enumMethods(self, ns, name):
        return {
            "equals": [
                {
                    "argTypes": [
                        "Object"
                    ],
                    "isStatic": False,
                    "methodDoc": None,
                    "name": "equals",
                    "parameters": [
                        {
                            "name": "obj",
                            "type": "Object"
                        }
                    ],
                    "references": [],
                    "returnType": "Boolean"
                }
            ],
            "hashcode": [
                {
                    "argTypes": [],
                    "isStatic": False,
                    "methodDoc": None,
                    "name": "hashCode",
                    "parameters": [],
                    "references": [],
                    "returnType": "Integer"
                }
            ],
            "ordinal": [
                {
                    "argTypes": [],
                    "isStatic": False,
                    "methodDoc": None,
                    "name": "ordinal",
                    "parameters": [],
                    "references": [],
                    "returnType": "Integer"
                }
            ],
            "values": [
                {
                    "argTypes": [],
                    "isStatic": True,
                    "methodDoc": None,
                    "name": "values",
                    "parameters": [],
                    "references": [],
                    "returnType": "List<" + ns + "." + name + ">"
                }
            ]
        }

    def properties(self, lst):
        return [{'name': name, 'references': []} for name in lst]

    def fix(self, api_apex):
        if api_apex['schema']['classes'].get('aggregateresult', False):
            return
        print('Fixing missing & broken classes.')
        api_apex['schema']['classes']['aggregateresult'] = {
            "methods": {
                "get": [self.methodSignature(False, 'get', 'Object', [('String', 'fieldName')])]
            },
            "properties": [],
            "name": "AggregateResult"
        }
        api_apex['schema']['classes']['describesobjectresult']['methods'].pop('getfieldsets')
        api_apex['system']['classes']['installcontext'] = {
            "methods": {
                "organizationid": [self.methodSignature(False, 'organizationId', 'Id', [])],
                "installerid": [self.methodSignature(False, 'installerId', 'Id', [])],
                "isupgrade": [self.methodSignature(False, 'isUpgrade', 'Boolean', [])],
                "ispush": [self.methodSignature(False, 'isPush', 'Boolean', [])],
                "previousversion": [self.methodSignature(False, 'previousVersion', 'Version', [])]
            },
            "properties": [],
            "name": "InstallContext",
            "isInterface": True
        }
        api_apex['system']['classes']['uninstallcontext'] = {
            "methods": {
                "organizationid": [self.methodSignature(False, 'organizationId', 'Id', [])],
                "uninstallerid": [self.methodSignature(False, 'uninstallerid', 'Id', [])],
            },
            "properties": [],
            "name": "UninstallContext",
            "isInterface": True
        }
        api_apex['database']['classes']['allowscallouts'] = {
            "methods": {},
            "properties": [],
            "name": "AllowsCallouts",
            "isInterface": True
        }
        api_apex['database']['classes']['stateful'] = {
            "methods": {},
            "properties": [],
            "name": "Stateful",
            "isInterface": True
        }
        api_apex['system']['classes']['savepoint'] = {
            "methods": {},
            "properties": [],
            "name": "Savepoint",
            "isInterface": False
        }
        api_apex['system']['classes']['list']['methods']['iterator'][0]['returnType'] = 'Iterator'
        api_apex['system']['classes']['set']['methods']['iterator'][0]['returnType'] = 'Iterator'
        api_apex['system']['classes']['applicationreadwritemode'] = {
            "methods": self.enumMethods('System', 'ApplicationReadWriteMode'),
            "properties": self.properties(['DEFAULT', 'READ_ONLY']),
            "name": "ApplicationReadWriteMode",
            "isInterface": False
        }
        api_apex['system']['classes']['schema']['methods']['groupstructures'][0]['returnType'] = 'List<Schema.Describe​DataCategoryGroupStructureResult>'
        api_apex['connectapi']['classes']['contenthub']['methods']['getalloweditemtypes'][-1]['parameters'][-1]['type'] = 'ConnectApi.ContentHubItemType'
        api_apex['connectapi']['classes']['contenthub']['methods']['getalloweditemtypes'][-1]['argTypes'][-1] = 'ConnectApi.ContentHubItemType'
        api_apex['connectapi']['classes']['contenthub']['methods']['getalloweditemtypes'][-3]['parameters'][-1]['type'] = 'ConnectApi.ContentHubItemType'
        api_apex['connectapi']['classes']['contenthub']['methods']['getalloweditemtypes'][-3]['argTypes'][-1] = 'ConnectApi.ContentHubItemType'
        api_apex['process']['classes']['plugindescriberesult.parametertype'] = {
            "methods": self.enumMethods('Process', 'PluginDescribeResult.ParameterType'),
            "properties": self.properties(['BOOLEAN', 'DATE', 'DATETIME', 'DECIMAL', 'DOUBLE', 'FLOAT', 'ID', 'INTEGER', 'LONG', 'STRING']),
            "name": "ParameterType",
            "isInterface": False
        }

    # merge maps
    #

    def param(self, signaturePart):
        m = re.match('(.+?)([A-Za-z0-9_]+)$', signaturePart)
        t = m.group(1).strip().replace('<T>', '').replace('<T1,T2>', '')
        return {'name': m.group(2).strip(), 'type': t}

    def methodParams(self, signature):
        m = re.match('.*\((.*)\)', signature)
        paramsString = m.group(1)
        diamondDepth = 0
        markedString = ''
        for idx in range(len(paramsString)):
            if 0 == diamondDepth and ',' == paramsString[idx]:
                markedString += '$'
                continue
            elif '<' == paramsString[idx]:
                diamondDepth += 1
            elif '>' == paramsString[idx]:
                diamondDepth -= 1
            markedString += paramsString[idx]
        if '' == markedString:
            return []
        return [self.param(p) for p in list(filter(lambda x: x, markedString.split('$')))]

    def methodPre(self, signature):
        m = re.match(r'(.*)\(.*\)$', signature)
        preSignature = m.group(1)
        markedString = ''
        diamondDepth = 0
        for idx in range(len(preSignature)):
            if 0 == diamondDepth and ' ' == preSignature[idx]:
                markedString += '$'
                continue
            elif '<' == preSignature[idx]:
                diamondDepth += 1
            elif '>' == preSignature[idx]:
                diamondDepth -= 1
            markedString += preSignature[idx]
        if '' == markedString:
            parts = []
        parts = [s.strip() for s in markedString.split('$')]
        parts = list(filter(lambda x: x, parts))
        return parts

    def methodIdentifier(self, signature):
        m = re.match(r'.+?([A-Za-z0-9_]+)\s*(<.+>)?\s*\(.*\)$', signature)
        return m.group(1)

    # get structured signatures

    def methodParsedSignature(self, signature):
        parts = self.methodPre(signature)
        returnType = parts[-2]
        identifier = self.methodIdentifier(signature)
        params = self.methodParams(signature)
        static = 'static' in [s.lower() for s in parts]
        return {
            "returnType": returnType,
            "parameters": params,
            "methodDoc": None,
            "argTypes": [p['type'] for p in params],
            "name": identifier,
            "isStatic": static,
            "references": []
        }

    def ctorParsedSignature(self, signature):
        identifier = self.methodIdentifier(signature)
        params = self.methodParams(signature)
        return {
            "parameters": params,
            "name": identifier,
            "methodDoc": None,
            "references": []
        }

    def propertyParsedSignature(self, signature):
        m = re.match(r'.+?([A-Za-z0-9_]+)\s*\{.*\}$', signature)
        return {
            "name": m.group(1),
            "references": [],
        }

    def matchCtor(self, c1, c2):
        params1 = [p['type'].lower() for p in c1['parameters']]
        params2 = [p['type'].lower() for p in c2['parameters']]
        return params1 == params2

    def writeJsonDocs(self):
        self.load()  # load signatures from api reference
        # with open('doc-signatures0.json', 'w') as out:
        #     out.write(json.dumps(self.api_apex_doc, indent=4))
        self.load2()
        with open('doc-signatures.json', 'w') as out:
            out.write(json.dumps(self.api_apex_doc, indent=4))

    def fixMissing(self, name):
        with open(name, 'r') as inp:
            api_apex = json.loads(inp.read())

        if os.path.exists('doc-signatures.json'):
            with open('doc-signatures.json', 'r') as inp:
                self.api_apex_doc = json.loads(inp.read())
        else:
            self.writeJsonDocs()

        for (ns, nsdata) in self.api_apex_doc.items():
            if ns not in api_apex:
                print('Namespace "' + ns + '" was missing in completions.')
                api_apex[ns] = {'classes': {}, 'name': nsdata['name']}
            for (cname, cdata) in nsdata.get('classes', []).items():
                if cname not in api_apex[ns]['classes']:
                    print('Class "' + ns + ':' + cname + '" was missing in completions.')
                    clsId = cdata['name'].split('.', 1)[-1]
                    api_apex[ns]['classes'][cname] = {'name': clsId, 'methods': {}, 'properties': [], 'constructors': []}
                if cdata['isInterface'] and not api_apex[ns]['classes'][cname].get('isInterface'):
                    print('Interface info was incorrect for "' + ns + ':' + cname + '" in completions.')
                    api_apex[ns]['classes'][cname]['isInterface'] = cdata['isInterface']
                methodSignatures = {}
                for signature in cdata.get('methods', []):
                    psig = self.methodParsedSignature(signature)
                    psigs = methodSignatures.get(psig['name'].lower(), [])
                    psigs.append(psig)
                    methodSignatures[psig['name'].lower()] = psigs
                for (mname, psigs) in methodSignatures.items():
                    if mname not in api_apex[ns]['classes'][cname]['methods']:
                        print('Method "' + mname + '" in Class"' + ns + '.' + cname + '" was missing in completions.')
                        api_apex[ns]['classes'][cname]['methods'][mname] = psigs
                for signature in cdata.get('properties', []):
                    psig = self.propertyParsedSignature(signature)
                    if psig['name'].lower() not in [p['name'].lower() for p in api_apex[ns]['classes'][cname]['properties']]:
                        print('Property "' + psig['name'] + '" in Class"' + ns + '.' + cname + '" was missing in completions.')
                        api_apex[ns]['classes'][cname]['properties'].append(psig)
                for signature in cdata.get('constructors', []):
                    psig = self.ctorParsedSignature(signature)
                    if 1 != len(list(filter(lambda c2: self.matchCtor(psig, c2), api_apex[ns]['classes'][cname]['constructors']))):
                        print('Ctor "' + signature + '" in Class"' + ns + '.' + cname + '" was missing in completions.')
                        api_apex[ns]['classes'][cname]['constructors'].append(psig)
        self.fix(api_apex)
        with open(name, 'w') as out:
            out.write(json.dumps(api_apex, indent=4))


def loadProperties():
    with open('./build.properties', 'r') as f:
        configString = '[config]\n' + f.read()
    config = configparser.ConfigParser()
    config.read_string(configString)
    return config['config']


def getWriter():
    cnf = loadProperties()
    api = RestApi(cnf['sf.username'], cnf['sf.password']+cnf['sf.securitytoken'])
    return ApiInfoWriter(api)


parser = argparse.ArgumentParser()
parser.add_argument("-retrieve", help="load, restructure & save completions to api.json", action="store_true")
parser.add_argument("-interfaces", help="add interface infos to api.json", action="store_true")
parser.add_argument("-docsjson", help="load api info from docs and save to doc-signatures.json file", action="store_true")
parser.add_argument("-retrievedocs", help="load api info from docs & add to api.json", action="store_true")
parser.add_argument("-properties", help="add property types & static info to api.json", action="store_true")
parser.add_argument("-getwrapper", help="generate TypeWrapper class which generates Parents.txt", action="store_true")
parser.add_argument("-inheritance", help="add inheritance infos from Parents.txt to api.json", action="store_true")
parser.add_argument("-sobjects", help="load, restructure & save object metadata in schema.json", action="store_true")
args = parser.parse_args()
if args.retrieve:
    writer = getWriter()
    writer.writeApiApex('api.json')
elif args.interfaces:
    writer = getWriter()
    writer.findInterfaces('api.json')
elif args.docsjson:
    ex = ExtendApiFromDocumentation()
    ex.writeJsonDocs()
elif args.retrievedocs:
    ex = ExtendApiFromDocumentation()
    ex.fixMissing('api.json')
elif args.properties:
    writer = getWriter()
    writer.properties('api.json')
elif args.getwrapper:
    writer = getWriter()
    writer.genApexTypesClass('api.json')
elif args.inheritance:
    writer = getWriter()
    writer.addSuper('api.json', 'Parents.txt')
elif args.sobjects:
    writer = getWriter()
    writer.writeSObjectDescribe('schema.json')

