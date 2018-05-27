public with sharing class SuperCalculateController {

    public PageReference run() {
        TypeWrapper.Batch batch = new TypeWrapper.Batch();
        System.enqueueJob(batch);

        return new PageReference('/apexpages/setup/listAsyncApexJobs.apexp').setRedirect(true);
    }
}