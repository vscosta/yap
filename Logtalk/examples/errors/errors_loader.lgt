
:- initialization((
	catch(logtalk_load(lgtmthdredef, [report(on)]), _, true),
	catch(logtalk_load(invclause, [report(on)]), _, true),
	catch(logtalk_load(unknowndir, [report(on)]), _, true),
	catch(logtalk_load(noninstdir, [report(on)]), _, true),
	catch(logtalk_load(invargdir, [report(on)]), _, true),
	catch(logtalk_load(unmatchdir, [report(on)]), _, true),
	catch(logtalk_load(catdynpred, [report(on)]), _, true),
	catch(logtalk_load(ccredef, [report(on)]), _, true),
	catch(logtalk_load(usesrepeated, [report(on)]), _, true),
	catch(logtalk_load(usesconflict, [report(on)]), _, true))).
