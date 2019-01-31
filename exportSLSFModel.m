function [] = exportSLSFModel(chart,outfile)

    j = slsf2json(chart);
    fid = fopen(outfile,'wt');
    fprintf(fid,'%s',j);
    fclose(fid);