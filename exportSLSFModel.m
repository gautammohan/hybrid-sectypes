function [] = exportSLSFModel(chart,outfile)

    j = slsf2json(chart);
    fid = fopen([outfile '.json'],'wt');
    fprintf(fid,'%s',j);
    fclose(fid);


    ds = find(chart,'-isa','Stateflow.Data');
    fid2 = fopen([outfile '.sectypes'],'wt');
    for i=1:length(ds)
        fprintf(fid,'%s : Low\n',get(ds(i),'Name'));
    end
    fclose(fid2);