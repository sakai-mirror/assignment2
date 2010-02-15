package org.sakaiproject.assignment2.tool.taggable;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.sakaiproject.component.cover.ComponentManager;
import org.sakaiproject.taggable.api.TaggingHelperInfo;
import org.sakaiproject.taggable.api.TaggingManager;
import org.sakaiproject.taggable.api.TaggingProvider;

public class OSPHelperServlet extends HttpServlet {
    public static final String MATRIX_PROV = 
        "org.theospi.portfolio.tagging.api.MatrixTaggingProvider";
    
    @Override
    protected void service(HttpServletRequest req, HttpServletResponse res)
            throws ServletException, IOException {
        
        
        String helperId = "osp.matrix.link";
        
        String activityRef = 
            "/assignment/a/usedtools/4f7e695e-556a-4038-ad02-c4f24c6e47e8";

        TaggingManager taggingManager = (TaggingManager) ComponentManager
        .get("org.sakaiproject.taggable.api.TaggingManager");
        TaggingProvider provider = taggingManager.findProviderById(MATRIX_PROV);

        TaggingHelperInfo helperInfo = provider
            .getActivityHelperInfo(activityRef);
        
        
        
        // TODO Auto-generated method stub
        //super.service(req, res);
        //OutputStream out = res.getOutputStream();
        PrintWriter w = res.getWriter();
        w.write("<html><body><p>Asnn2 Matrix Link Servlet</p></body></html>");
    }
}
